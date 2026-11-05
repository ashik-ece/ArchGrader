# The instructor solution is run once through the same runner as student
# submissions, and its objects are cached so every check can compare against
# them. The cache lives in memory for the session and is keyed on the content
# of the solution and its data files, so editing either one invalidates it.
.reference_cache <- new.env(parent = emptyenv())

get_reference <- function(solution_path, data_dir = NULL, timeout = 30) {
  if (!file.exists(solution_path)) {
    stop("Instructor solution not found: ", solution_path, call. = FALSE)
  }
  data_files <- if (is.null(data_dir)) character() else list.files(data_dir, full.names = TRUE)
  data_files <- data_files[!dir.exists(data_files)]
  key <- paste(unname(tools::md5sum(c(solution_path, data_files))), collapse = "|")

  if (!is.null(.reference_cache[[key]])) return(.reference_cache[[key]])

  ref <- run_submission(solution_path, data_dir = data_dir, timeout = timeout)
  if (!is.null(ref$error)) {
    stop("The instructor solution '", basename(solution_path), "' failed",
         if (!is.null(ref$error_chunk)) paste0(" in chunk '", ref$error_chunk, "'"),
         ": ", ref$error, call. = FALSE)
  }
  assign(key, ref, envir = .reference_cache)
  ref
}

clear_reference_cache <- function() {
  rm(list = ls(.reference_cache, all.names = TRUE), envir = .reference_cache)
  invisible(NULL)
}
