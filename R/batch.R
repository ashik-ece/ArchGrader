#' Grade every submission in a folder
#'
#' Grades each `.qmd` in `folder` (not recursively) and never stops on one
#' bad submission: a crashing, looping or unreadable file becomes a normal
#' row and report. Writes `<output_dir>/scores.csv` with one row per
#' submission, and one feedback report per submission in
#' `<output_dir>/reports/`, then prints a short summary.
#'
#' Files are processed in sorted order, so re-running the same folder gives an
#' identical CSV apart from the `seconds` column.
#'
#' Problems on the instructor's side (a broken rubric or solution, or Quarto
#' missing when reports are wanted) stop the batch before any student is
#' graded, because they would affect every student.
#'
#' @section scores.csv columns:
#' `student_id`, `total` (points earned), one column per rubric criterion
#' (points earned; blank for `manual` criteria), `error` (`TRUE` if the code
#' failed), `error_chunk` (label of the failing chunk, blank if none) and
#' `seconds` (time for this submission, including its report).
#'
#' @inheritParams grade_submission
#' @param folder Folder of student `.qmd` files.
#' @param output_dir Where `scores.csv` and `reports/` are written.
#' @param reports Write a feedback report per submission.
#' @param markdown Also write each report as Markdown.
#' @param quiet Suppress the console summary.
#' @return Invisibly, a list with `scores` (the data frame written to the CSV),
#'   `results` (one [grade_submission()] result per file, named by student ID),
#'   `scores_path`, `report_paths` and `summary` (counts and timing).
#' @export
grade_folder <- function(folder, assignment = "a1", output_dir = "grading-output",
                         reports = TRUE, markdown = FALSE, rubric = NULL,
                         solution = NULL, data_dir = NULL, timeout = NULL,
                         quiet = FALSE) {
  if (!dir.exists(folder)) stop("Folder not found: ", folder, call. = FALSE)
  files <- list.files(folder, pattern = "\\.qmd$", ignore.case = TRUE, full.names = TRUE)
  files <- files[order(basename(files), method = "radix")]
  if (length(files) == 0) stop("No .qmd files found in ", folder, call. = FALSE)

  if (isTRUE(reports) && !quarto_available()) {
    stop("Quarto was not found, so feedback reports cannot be written. Install it from ",
         "https://quarto.org and put `quarto` on the PATH, or use reports = FALSE.", call. = FALSE)
  }

  # Instructor-side problems stop here, before the first student.
  setup <- resolve_assignment(assignment, rubric, solution, data_dir, timeout)
  get_reference(setup$solution, data_dir = setup$data_dir, timeout = setup$timeout)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  reports_dir <- file.path(output_dir, "reports")
  batch_start <- proc.time()[["elapsed"]]

  results <- list()
  rows <- list()
  report_paths <- character()
  report_failures <- character()

  for (f in files) {
    t0 <- proc.time()[["elapsed"]]
    res <- grade_submission(f, assignment = assignment, rubric = setup$rubric$path,
                            solution = setup$solution, data_dir = setup$data_dir,
                            timeout = setup$timeout)
    if (isTRUE(reports)) {
      written <- tryCatch(
        write_feedback(res, reports_dir, markdown = markdown),
        error = function(e) {
          report_failures <<- c(report_failures, res$student_id)
          warning("Report for ", res$student_id, " failed: ", conditionMessage(e), call. = FALSE)
          character()
        }
      )
      report_paths <- c(report_paths, written)
    }
    seconds <- proc.time()[["elapsed"]] - t0

    results[[res$student_id]] <- res
    rows[[length(rows) + 1]] <- score_row(res, seconds)
  }

  scores <- do.call(rbind, rows)
  scores_path <- file.path(output_dir, "scores.csv")
  utils::write.csv(scores, scores_path, row.names = FALSE, na = "", eol = "\n",
                   fileEncoding = "UTF-8")

  possible <- results[[1]]$possible
  summary <- list(
    n = nrow(scores),
    average_total = round(mean(scores$total), 2),
    possible = possible,
    average_percent = if (possible > 0) round(100 * mean(scores$total) / possible, 1) else NA_real_,
    n_errors = sum(scores$error),
    total_seconds = round(proc.time()[["elapsed"]] - batch_start, 1),
    median_seconds = round(stats::median(scores$seconds), 2),
    report_failures = report_failures
  )
  if (!quiet) print_batch_summary(summary, scores_path, if (isTRUE(reports)) reports_dir else NULL)

  invisible(list(scores = scores, results = results, scores_path = scores_path,
                 report_paths = report_paths, summary = summary))
}

# One row of scores.csv.
score_row <- function(res, seconds) {
  tab <- res$table
  criteria <- as.list(stats::setNames(tab$earned, tab$id))
  row <- c(
    list(student_id = res$student_id, total = res$earned),
    criteria,
    list(error = isTRUE(res$error),
         error_chunk = res$run$error_chunk %||% NA_character_,
         seconds = round(seconds, 2))
  )
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

print_batch_summary <- function(s, scores_path, reports_dir) {
  cat("\nArchGrader batch summary\n")
  cat("  Graded:         ", s$n, if (s$n == 1) " submission\n" else " submissions\n", sep = "")
  cat("  Average score:  ", fmt_num(s$average_total), " / ", fmt_num(s$possible),
      if (!is.na(s$average_percent)) paste0(" (", s$average_percent, "%)") else "", "\n", sep = "")
  cat("  With errors:    ", s$n_errors, "\n", sep = "")
  cat("  Total time:     ", s$total_seconds, " s (median ", s$median_seconds, " s per submission)\n", sep = "")
  cat("  Scores:         ", scores_path, "\n", sep = "")
  if (!is.null(reports_dir)) cat("  Reports:        ", reports_dir, "/\n", sep = "")
  if (length(s$report_failures)) {
    cat("  Reports failed: ", paste(s$report_failures, collapse = ", "), "\n", sep = "")
  }
  invisible(NULL)
}
