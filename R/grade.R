#' Grade one submission
#'
#' The single entry point: runs the submission, checks every rubric
#' criterion against the instructor solution, and scores it. The batch runner
#' and the feedback report are built on this function.
#'
#' The instructor solution is run once and cached for the session, so grading
#' many submissions only pays for the solution once.
#'
#' A submission that cannot be run at all (for example a damaged file) still
#' returns a normal result: every criterion scores zero and `error` is `TRUE`.
#' Problems on the instructor's side (a broken rubric or solution) stop with
#' an error instead, because they would affect every student.
#'
#' @param submission Path to the student's `.qmd`.
#' @param assignment Assignment name, such as `"a1"`. Selects the rubric
#'   `inst/rubrics/<assignment>.yaml` shipped with the package.
#' @param rubric Optional path to a rubric file, to use instead of the
#'   packaged one.
#' @param solution Optional path to the instructor solution. Defaults to the
#'   file named in the rubric, inside the packaged `assignments` folder.
#' @param data_dir Optional folder with the assignment's data files. Defaults
#'   to the packaged `assignments` folder.
#' @param timeout Seconds allowed for the submission. Defaults to the
#'   rubric's `timeout`, or 30.
#' @return A list of class `archgrader_result` with `student_id`,
#'   `assignment`, `rubric`, `run` (the runner result), `table` (one row per
#'   criterion), `earned`, `possible`, `review_points`, `percent`,
#'   `error` (`TRUE` if the code failed) and `seconds`.
#' @export
grade_submission <- function(submission, assignment = "a1", rubric = NULL,
                             solution = NULL, data_dir = NULL, timeout = NULL) {
  start <- proc.time()[["elapsed"]]
  if (!is.character(submission) || length(submission) != 1 || !file.exists(submission)) {
    stop("Submission file not found: ", format(submission), call. = FALSE)
  }

  setup <- resolve_assignment(assignment, rubric, solution, data_dir, timeout)
  rub <- setup$rubric
  reference <- get_reference(setup$solution, data_dir = setup$data_dir, timeout = setup$timeout)

  student <- tryCatch(
    run_submission(submission, data_dir = setup$data_dir, timeout = setup$timeout),
    error = function(e) unreadable_run(conditionMessage(e))
  )

  results <- lapply(rub$criteria, run_check, student = student, reference = reference)
  scored <- score_criteria(rub, results)

  structure(
    list(
      student_id = tools::file_path_sans_ext(basename(submission)),
      assignment = rub$assignment,
      rubric = rub,
      run = student,
      table = scored$table,
      earned = scored$earned,
      possible = scored$possible,
      review_points = scored$review_points,
      percent = scored$percent,
      error = !is.null(student$error),
      seconds = proc.time()[["elapsed"]] - start
    ),
    class = "archgrader_result"
  )
}

# Locate and load the rubric, solution and data for an assignment. Rubric
# problems stop here, before any student is graded.
resolve_assignment <- function(assignment, rubric = NULL, solution = NULL,
                               data_dir = NULL, timeout = NULL) {
  assignments_dir <- system.file("assignments", package = "ArchGrader")
  if (is.null(rubric)) {
    rubric <- system.file("rubrics", paste0(assignment, ".yaml"), package = "ArchGrader")
    if (!nzchar(rubric)) {
      stop("No packaged rubric for assignment '", assignment, "'.", call. = FALSE)
    }
  }
  rub <- load_rubric(rubric)
  list(
    rubric = rub,
    data_dir = data_dir %||% assignments_dir,
    solution = solution %||% file.path(assignments_dir, rub$solution),
    timeout = timeout %||% rub$timeout %||% 30
  )
}

# What the checks see when a file could not be run at all. Everything scores
# zero and the report shows the problem.
unreadable_run <- function(message) {
  list(
    objects = list(), object_chunk = character(),
    error = paste0("The submission file could not be read: ", message),
    error_chunk = NULL, timed_out = FALSE, elapsed = 0,
    chunks = character(), code = ""
  )
}

#' @export
print.archgrader_result <- function(x, ...) {
  cat("ArchGrader: ", x$student_id, " | ", x$rubric$title, "\n", sep = "")
  tab <- x$table
  show <- data.frame(
    criterion = tab$id,
    status = tab$status,
    credit = ifelse(is.na(tab$credit), "-", formatC(tab$credit, format = "f", digits = 2)),
    points = ifelse(is.na(tab$earned), paste0("review/", tab$points),
                    paste0(formatC(tab$earned, format = "f", digits = 2), "/", tab$points)),
    stringsAsFactors = FALSE
  )
  print(show, row.names = FALSE, right = FALSE)
  cat("\nScore: ", format(x$earned, nsmall = 2), " / ", x$possible, " (", x$percent, "%)", sep = "")
  if (x$review_points > 0) cat("  +", x$review_points, "points need TA review")
  cat("\n")
  if (x$error) {
    cat("Error in chunk `", x$run$error_chunk %||% "?", "`: ", x$run$error, "\n", sep = "")
  }
  invisible(x)
}
