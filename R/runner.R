#' Run a student submission in a clean R process
#'
#' Extracts the R code from a `.qmd` (keeping each chunk's label), runs the
#' chunks top to bottom in a fresh R process with a time limit, and returns
#' the objects they created. Errors never propagate: a failing submission
#' returns normally, with the error message and the label of the chunk where
#' it happened. Objects created before the failure are still returned.
#'
#' Chunk labels are read from `{r label}` headers and from Quarto-style
#' `#| label: name` lines. Unlabeled chunks are named `unnamed-chunk-1`,
#' `unnamed-chunk-2`, and so on. Chunks with `eval: false` are not run.
#'
#' Each run uses its own temporary working directory containing a copy of the
#' files in `data_dir`, so submissions that read `artifacts.csv` work and
#' nothing a submission writes touches the project.
#'
#' This is a separate process with a timeout, not a sandbox. Running
#' untrusted code in a live course should happen inside a container.
#'
#' @param path Path to a `.qmd` file.
#' @param data_dir Directory whose non-`.qmd` files (the data) are copied into
#'   the working directory of the run. `NULL` for none.
#' @param timeout Seconds before the run is stopped.
#' @return A list with
#'   * `objects`: named list of the objects created,
#'   * `object_chunk`: named character vector, the chunk that last created or
#'     changed each object,
#'   * `error`: error message, or `NULL`,
#'   * `error_chunk`: label of the failing (or timed-out) chunk, or `NULL`,
#'   * `timed_out`: `TRUE` if the timeout stopped the run,
#'   * `elapsed`: seconds taken,
#'   * `chunks`: labels of all chunks, in order,
#'   * `code`: the extracted code as one string (for `uses_function` checks).
#' @export
run_submission <- function(path, data_dir = NULL, timeout = 30) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("Submission file not found: ", format(path), call. = FALSE)
  }
  chunks <- extract_chunks(path)

  workdir <- tempfile("archgrader-run-")
  dir.create(workdir)
  state_file <- tempfile("state-", fileext = ".rds")
  current_file <- tempfile("current-", fileext = ".txt")
  on.exit(unlink(c(workdir, state_file, current_file), recursive = TRUE), add = TRUE)

  if (!is.null(data_dir)) {
    files <- list.files(data_dir, full.names = TRUE)
    files <- files[!grepl("\\.qmd$", files, ignore.case = TRUE) & !dir.exists(files)]
    file.copy(files, workdir)
  }

  start <- proc.time()[["elapsed"]]
  outcome <- tryCatch(
    callr::r(
      run_chunks_in_child,
      args = list(chunks = chunks, workdir = workdir,
                  state_file = state_file, current_file = current_file),
      timeout = timeout
    ),
    callr_timeout_error = function(e) list(status = "timeout"),
    error = function(e) {
      list(status = "crashed", message = clean_crash_message(e))
    }
  )
  elapsed <- proc.time()[["elapsed"]] - start

  # A submission that calls quit() exits cleanly, so callr returns NULL rather
  # than an error. Anything without a status means the child never finished.
  if (!is.list(outcome) || is.null(outcome$status)) {
    outcome <- list(status = "crashed", message = clean_crash_message(simpleError("ended unexpectedly")))
  }

  state <- if (file.exists(state_file)) readRDS(state_file) else list(objects = list(), owner = character())
  running <- if (file.exists(current_file)) readLines(current_file, warn = FALSE)[1] else NA_character_

  error <- NULL
  error_chunk <- NULL
  timed_out <- FALSE
  if (identical(outcome$status, "timeout")) {
    timed_out <- TRUE
    error_chunk <- running
    error <- paste0("The code did not finish within ", format(timeout), " seconds ",
                    "(possible infinite loop or very slow code).")
  } else if (identical(outcome$status, "crashed")) {
    error_chunk <- running
    error <- outcome$message
  } else if (identical(outcome$status, "error")) {
    error_chunk <- outcome$chunk
    error <- outcome$message
  }
  if (!is.null(error_chunk) && is.na(error_chunk)) error_chunk <- NULL

  list(
    objects = state$objects,
    object_chunk = state$owner,
    error = error,
    error_chunk = error_chunk,
    timed_out = timed_out,
    elapsed = elapsed,
    chunks = names(chunks),
    code = paste(unlist(chunks), collapse = "\n\n")
  )
}

# Runs inside the child R process. callr resets this function's environment to
# the global environment, so it must use only base R and `pkg::name` calls.
# Progress is written to disk after every chunk, so a timeout or a crash still
# leaves the objects created so far, and the label of the running chunk.
run_chunks_in_child <- function(chunks, workdir, state_file, current_file) {
  setwd(workdir)
  env <- new.env(parent = globalenv())
  owner <- character()

  save_state <- function() {
    tmp <- paste0(state_file, ".tmp")
    ok <- tryCatch({
      saveRDS(list(objects = mget(ls(env), envir = env), owner = owner), tmp)
      file.rename(tmp, state_file)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    invisible(ok)
  }
  save_state()

  for (i in seq_along(chunks)) {
    label <- names(chunks)[i]
    writeLines(label, current_file)

    # State is saved after every top-level expression (also when it fails), so
    # a timeout in the middle of a chunk still keeps the answers computed
    # before the loop started.
    message <- tryCatch({
      exprs <- parse(text = chunks[[i]], keep.source = FALSE)
      for (e in exprs) {
        before <- mget(ls(env), envir = env)
        tryCatch(eval(e, env), finally = {
          after <- mget(ls(env), envir = env)
          for (nm in names(after)) {
            if (!(nm %in% names(before)) || !identical(after[[nm]], before[[nm]])) owner[nm] <- label
          }
          save_state()
        })
      }
      NULL
    }, error = function(e) conditionMessage(e))

    if (!is.null(message)) {
      return(list(status = "error", message = message, chunk = label))
    }
  }
  list(status = "ok")
}

# The message of a callr failure, without the process-management noise.
clean_crash_message <- function(e) {
  msg <- conditionMessage(e)
  if (grepl("exited|crash|terminated|segfault|unexpectedly", msg, ignore.case = TRUE)) {
    return("The R process ended unexpectedly (for example, the code called quit() or crashed R).")
  }
  msg
}

# Pull the R chunks out of a .qmd as an ordered, named list of code strings.
# Names are the chunk labels.
extract_chunks <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- move_option_labels_to_headers(lines)

  src <- tempfile(fileext = ".qmd")
  out <- tempfile(fileext = ".R")
  on.exit(unlink(c(src, out)), add = TRUE)
  writeLines(lines, src, useBytes = TRUE)
  suppressMessages(knitr::purl(src, output = out, documentation = 1, quiet = TRUE))

  script <- readLines(out, warn = FALSE, encoding = "UTF-8")
  header <- grepl("^## -{4}", script)
  if (!any(header)) return(stats::setNames(list(), character()))

  idx <- which(header)
  ends <- c(idx[-1] - 1L, length(script))
  labels <- vapply(script[idx], parse_purl_header, character(1), USE.NAMES = FALSE)

  unnamed <- labels == ""
  labels[unnamed] <- paste0("unnamed-chunk-", seq_len(sum(unnamed)))

  code <- Map(function(s, e) {
    body <- if (e >= s + 1L) script[(s + 1L):e] else character()
    paste(trimws(body, which = "right"), collapse = "\n")
  }, idx, ends)
  stats::setNames(code, labels)
}

# "## ----keep, echo=FALSE-----" -> "keep"; "## -------" -> ""
parse_purl_header <- function(line) {
  inner <- sub("^## -{4}", "", line)
  inner <- sub("-+$", "", inner)
  label <- strsplit(inner, ",", fixed = TRUE)[[1]][1]
  if (is.na(label)) "" else trimws(label)
}

# knitr::purl() keeps a chunk label only when it is in the header
# (```{r label}), so rewrite Quarto-style `#| label: name` lines into the
# header form before extraction.
move_option_labels_to_headers <- function(lines) {
  i <- 1L
  while (i < length(lines)) {
    m <- regmatches(lines[i], regexec("^(\\s*```+\\s*\\{r)(.*)\\}\\s*$", lines[i]))[[1]]
    if (length(m) == 3) {
      rest <- trimws(m[3])
      has_label <- nzchar(rest) && !startsWith(rest, ",") && !grepl("^[A-Za-z_.]+\\s*=", rest)
      nxt <- regmatches(lines[i + 1L], regexec("^#\\|\\s*label:\\s*(\\S+)\\s*$", lines[i + 1L]))[[1]]
      if (!has_label && length(nxt) == 2) {
        opts <- if (nzchar(rest)) paste0(", ", sub("^,\\s*", "", rest)) else ""
        lines[i] <- paste0(m[2], " ", nxt[2], opts, "}")
        lines <- lines[-(i + 1L)]
      }
    }
    i <- i + 1L
  }
  lines
}
