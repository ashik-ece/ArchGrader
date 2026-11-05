# Feedback report
#
# Feedback is assembled, not generated: the rubric holds a comment for pass,
# partial and fail on each criterion plus a hint, and the grader fills in the
# observed and expected values, the object name and the chunk label. Identical
# mistakes therefore get identical feedback.
#
# The work is split in two. build_feedback_model() (here, in R, unit tested)
# turns a grading result into plain markdown strings. The Quarto template
# (inst/templates/feedback.qmd) owns the layout and only stitches those
# strings together, using base R and knitr, so it renders even when the
# package is not installed.
#
# Student-controlled text (error messages, values, chunk labels) is untrusted:
# it ends up in a report an instructor opens in a browser. It is always
# escaped before it reaches markdown, so it cannot inject HTML or markdown.

STATUS_LABELS <- c(
  pass = "\u2713 Correct",
  partial = "\u25D0 Partly correct",
  fail = "\u2717 Not correct yet",
  needs_review = "\u2691 Needs TA review"
)

# --- escaping --------------------------------------------------------------

# Escape text so pandoc renders it literally. Pandoc allows a backslash before
# any ASCII punctuation character, which covers *, _, `, <, >, [, ], $, #, | and
# friends. Line breaks become spaces so a value cannot start a new block.
md_escape <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x))
  gsub("([[:punct:]])", "\\\\\\1", x)
}

# Text for the inside of a `code span`: it cannot contain a backtick.
code_safe <- function(x) {
  x <- gsub("\\s+", " ", as.character(x))
  gsub("`", "'", x, fixed = TRUE)
}

# Escape instructor-written or check-generated text that mixes code spans and
# plain text: code spans are sanitised, the rest is escaped.
escape_mixed <- function(text) {
  map_segments(text, plain = md_escape, code = function(s) paste0("`", code_safe(s), "`"))
}

# Apply a function to the code spans (`...`) and the plain text between them.
map_segments <- function(text, plain, code) {
  spans <- regmatches(text, gregexpr("`[^`]*`", text))[[1]]
  rest <- regmatches(text, gregexpr("`[^`]*`", text), invert = TRUE)[[1]]
  out <- character()
  for (i in seq_along(rest)) {
    out <- c(out, plain(rest[i]))
    if (i <= length(spans)) out <- c(out, code(sub("^`(.*)`$", "\\1", spans[i])))
  }
  paste(out, collapse = "")
}

#' Fill the rubric placeholders in a comment or hint
#'
#' `values` holds raw, possibly student-controlled text for `object`,
#' `observed`, `expected` and `chunk`. Inside a code span the value is
#' sanitised for code; elsewhere it is escaped. The rubric's own wording is
#' kept as written, so it can use code spans.
#' @noRd
fill_text <- function(text, values) {
  get_value <- function(name) {
    v <- values[[name]]
    if (is.null(v) || length(v) == 0 || is.na(v)) "" else as.character(v)
  }
  pattern <- "\\{(object|observed|expected|chunk)\\}"
  fill <- function(segment, convert) {
    m <- gregexpr(pattern, segment)
    regmatches(segment, m) <- lapply(regmatches(segment, m), function(found) {
      vapply(gsub("[{}]", "", found), function(nm) convert(get_value(nm)), "")
    })
    segment
  }
  map_segments(
    text,
    plain = function(s) fill(s, md_escape),
    code = function(s) paste0("`", fill(s, code_safe), "`")
  )
}

# A fence long enough that no run of backticks in `text` can close it.
fenced_block <- function(text) {
  runs <- regmatches(text, gregexpr("`+", text))[[1]]
  n <- max(3L, nchar(runs) + 1L)
  fence <- strrep("`", n)
  paste0(fence, "\n", gsub("[\r\n]+", "\n", text), "\n", fence)
}

fmt_num <- function(x) format(round(x, 2), trim = TRUE, drop0trailing = TRUE, nsmall = 0)

# --- model -----------------------------------------------------------------

#' Turn a grading result into the pieces of a feedback report
#' @param result A result from [grade_submission()].
#' @return A list of plain markdown strings and small data frames.
#' @noRd
build_feedback_model <- function(result) {
  tab <- result$table
  crit <- result$rubric$criteria
  scored <- tab$status != "needs_review"

  # one-line summary
  n_ok <- sum(tab$status == "pass")
  summary <- paste0(
    "You earned **", fmt_num(result$earned), " of ", fmt_num(result$possible), " points**",
    if (!is.na(result$percent)) paste0(" (", fmt_num(result$percent), "%)") else "",
    ". ", n_ok, " of ", sum(scored), " scored ",
    if (sum(scored) == 1) "criterion is" else "criteria are", " fully correct."
  )
  if (result$review_points > 0) {
    summary <- paste0(summary, " A further ", fmt_num(result$review_points),
                      " points will be marked by a TA.")
  }

  # error section
  error <- NULL
  if (isTRUE(result$error)) {
    run <- result$run
    where <- if (!is.null(run$error_chunk)) paste0("chunk `", code_safe(run$error_chunk), "`") else "your code"
    lead <- if (isTRUE(run$timed_out)) {
      paste0("Your code was still running when the time limit was reached, in ", where,
             ". It was stopped. This usually means an endless loop or very slow code.")
    } else {
      paste0("Your code stopped with an error in ", where, ".")
    }
    error <- list(
      title = if (isTRUE(run$timed_out)) "Your code did not finish" else "Your code stopped with an error",
      text = paste0(lead, " Code after this point did not run, so answers that depend on it ",
                    "could not be checked. Fix this first."),
      message = fenced_block(run$error),
      chunk = run$error_chunk,
      timed_out = isTRUE(run$timed_out)
    )
  }

  # table of criteria
  points_text <- ifelse(
    tab$status == "needs_review", paste0("TA review / ", fmt_num(tab$points)),
    paste0(vapply(tab$earned, fmt_num, ""), " / ", vapply(tab$points, fmt_num, ""))
  )
  table <- data.frame(
    Criterion = tab$description,
    Points = points_text,
    Result = unname(STATUS_LABELS[tab$status]),
    stringsAsFactors = FALSE
  )

  # one block per scored criterion
  blocks <- list()
  review <- list()
  for (i in seq_len(nrow(tab))) {
    cr <- crit[[i]]
    row <- tab[i, ]
    if (row$status == "needs_review") {
      review[[length(review) + 1]] <- list(
        title = cr$description,
        points = fmt_num(row$points),
        hint = if (!is.null(cr$hint)) fill_text(cr$hint, list()) else NULL
      )
      next
    }
    values <- list(object = row$object, observed = row$observed,
                   expected = row$expected, chunk = row$chunk)
    comment_template <- cr$comments[[row$status]]
    comment <- fill_text(comment_template, values)
    lost <- row$status %in% c("partial", "fail")

    # Show the grader's detail only when the comment does not already say it.
    uses_values <- grepl("\\{(observed|expected)\\}", comment_template)
    details <- if (lost && row$check == "value" && !uses_values) escape_mixed(row$note) else NULL

    hint <- if (lost) fill_text(cr$hint, values) else NULL
    mentions_chunk <- !is.null(cr$hint) && grepl("\\{chunk\\}", cr$hint)
    where <- if (lost && !is.na(row$chunk) && !mentions_chunk) {
      paste0("Chunk to revisit: `", code_safe(row$chunk), "`")
    } else NULL

    blocks[[length(blocks) + 1]] <- list(
      title = cr$description,
      status = row$status,
      status_label = unname(STATUS_LABELS[row$status]),
      points = paste0(fmt_num(row$earned), " / ", fmt_num(row$points)),
      comment = comment, details = details, hint = hint, where = where
    )
  }

  list(
    student_id = result$student_id,
    assignment = result$assignment,
    title = result$rubric$title,
    earned = fmt_num(result$earned),
    possible = fmt_num(result$possible),
    percent = if (is.na(result$percent)) "" else paste0(fmt_num(result$percent), "%"),
    summary = summary,
    error = error,
    table = table,
    blocks = blocks,
    review = review
  )
}

# --- rendering -------------------------------------------------------------

quarto_available <- function() !is.null(quarto::quarto_path())

#' Write the feedback report for one graded submission
#'
#' Renders the Quarto template `inst/templates/feedback.qmd` for one result
#' and saves it as `<output_dir>/<student_id>.html`. The HTML is a single
#' self-contained file. Needs Quarto installed and on the `PATH`.
#'
#' @param result A result from [grade_submission()].
#' @param output_dir Folder for the report; created if missing.
#' @param markdown Also write `<student_id>.md`.
#' @return The path(s) written, invisibly.
#' @export
write_feedback <- function(result, output_dir = "reports", markdown = FALSE) {
  if (!inherits(result, "archgrader_result")) {
    stop("`result` must come from grade_submission().", call. = FALSE)
  }
  if (!quarto_available()) {
    stop("Quarto was not found. Install it from https://quarto.org and make sure ",
         "`quarto` is on the PATH (or set QUARTO_PATH).", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  template <- system.file("templates", "feedback.qmd", package = "ArchGrader")
  if (!nzchar(template)) stop("The feedback template is missing from the package.", call. = FALSE)

  work <- tempfile("archgrader-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(template, file.path(work, "feedback.qmd"))
  model_file <- file.path(work, "model.rds")
  saveRDS(build_feedback_model(result), model_file)

  render <- function(format, ext) {
    quarto::quarto_render(
      file.path(work, "feedback.qmd"),
      output_format = format,
      output_file = paste0("feedback.", ext),
      execute_params = list(student_id = result$student_id,
                            assignment = result$assignment,
                            model_rds = model_file),
      quiet = TRUE
    )
    out <- file.path(output_dir, paste0(result$student_id, ".", ext))
    if (!file.copy(file.path(work, paste0("feedback.", ext)), out, overwrite = TRUE)) {
      stop("Could not write the report to ", out, call. = FALSE)
    }
    out
  }

  paths <- render("html", "html")
  if (isTRUE(markdown)) paths <- c(paths, render("gfm", "md"))
  invisible(paths)
}
