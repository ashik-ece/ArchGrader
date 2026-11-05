sol_path <- function() system.file("assignments", "a1_solution.qmd", package = "ArchGrader")
mock_dir <- function() system.file("mock_submissions", "a1", package = "ArchGrader")

test_that("the instructor solution scores 100%", {
  r <- grade_submission(sol_path())
  expect_s3_class(r, "archgrader_result")
  expect_equal(r$earned, 100)
  expect_equal(r$percent, 100)
  expect_false(r$error)
  expect_true(all(r$table$status == "pass"))
  expect_equal(r$student_id, "a1_solution")
})

test_that("grading is deterministic: same input, same table", {
  p <- file.path(mock_dir(), "a1_partial_table.qmd")
  a <- grade_submission(p)
  b <- grade_submission(p)
  expect_identical(a$table, b$table)
  expect_identical(a$earned, b$earned)
})

test_that("the hand-made broken copies score exactly what was predicted beforehand", {
  key <- read.csv(system.file("mock_submissions", "expected_scores.csv", package = "ArchGrader"))
  planted <- read.csv(system.file("mock_submissions", "planted_mistakes.csv", package = "ArchGrader"),
                      na.strings = character())
  for (sub in unique(key$submission[key$assignment == "a1"])) {
    r <- grade_submission(file.path(mock_dir(), paste0(sub, ".qmd")))
    want <- key[key$submission == sub, ]
    got <- stats::setNames(r$table$earned, r$table$id)
    expect_equal(unname(got), unname(stats::setNames(want$expected_points, want$criterion)[names(got)]),
                 info = sub)
    expected_chunk <- planted$error_chunk[planted$submission == sub]
    if (nzchar(expected_chunk)) {
      expect_equal(r$run$error_chunk, expected_chunk, info = sub)
    } else {
      expect_null(r$run$error_chunk, info = sub)
    }
  }
})

test_that("a crashing submission still gets a full result, with the failing chunk", {
  r <- grade_submission(file.path(mock_dir(), "a1_error_in_chunk.qmd"))
  expect_true(r$error)
  expect_equal(r$run$error_chunk, "median-by-site")
  expect_equal(r$table$status[r$table$id == "renders"], "fail")
  expect_equal(r$table$chunk[r$table$id == "renders"], "median-by-site")
  expect_equal(r$table$status[r$table$id == "mean_length"], "pass")
})

test_that("a missing object scores zero and the result names it", {
  r <- grade_submission(file.path(mock_dir(), "a1_missing_object.qmd"))
  row <- r$table[r$table$id == "count_by_material", ]
  expect_equal(row$earned, 0)
  expect_equal(row$object, "count_by_material")
  expect_equal(row$chunk, "count-by-material")
  expect_match(row$note, "was not created")
})

test_that("an infinite loop is stopped and graded, not hung", {
  p <- write_qmd(c("load-data" = "artifacts <- read.csv('artifacts.csv')",
                   "overall-stats" = "mean_length <- mean(artifacts$length_mm)\nwhile (TRUE) {}"))
  r <- grade_submission(p, timeout = 3)
  expect_true(r$run$timed_out)
  expect_equal(r$table$status[r$table$id == "renders"], "fail")
  expect_equal(r$table$status[r$table$id == "mean_length"], "pass")
})

test_that("an empty submission scores zero without crashing", {
  p <- write_qmd(c(a = "x <- 1"))
  r <- grade_submission(p)
  expect_equal(r$earned, 10)
  expect_equal(r$table$status[r$table$id == "mean_length"], "fail")
})

test_that("a different but valid method (base R) earns full credit", {
  p <- write_qmd(c(
    "load-data" = "artifacts <- read.csv('artifacts.csv')",
    "overall-stats" = "mean_length <- sum(artifacts$length_mm) / nrow(artifacts)\nsd_length <- sqrt(var(artifacts$length_mm))",
    "median-by-site" = "median_by_site <- aggregate(length_mm ~ site, artifacts, median)\nnames(median_by_site) <- c('site', 'median_length')",
    "count-by-material" = "count_by_material <- data.frame(table(raw_material = artifacts$raw_material), check.names = FALSE)\nnames(count_by_material)[2] <- 'n'",
    "summary-table" = paste(
      "summary_table <- do.call(rbind, lapply(split(artifacts, artifacts$site), function(d)",
      "  data.frame(site = d$site[1], n = nrow(d), mean_length = mean(d$length_mm), sd_length = sd(d$length_mm))))",
      sep = "\n")
  ))
  r <- grade_submission(p)
  expect_equal(r$earned, 100)
})

test_that("a broken rubric fails loudly before anything is graded", {
  bad <- rubric_list()
  bad$criteria[[1]]$check <- "valeu"
  expect_error(grade_submission(sol_path(), rubric = write_rubric(bad)), "unknown check type")
})

test_that("an unknown assignment is a clear error", {
  expect_error(grade_submission(sol_path(), assignment = "zzz"), "No packaged rubric")
})

test_that("a solution that fails is reported as the instructor's problem", {
  dir <- withr::local_tempdir()
  broken <- write_qmd(c(a = "stop('solution is broken')"), dir = dir, name = "sol.qmd")
  rub <- write_rubric(rubric_list(), dir = dir)
  expect_error(grade_submission(sol_path(), rubric = rub, solution = broken, data_dir = dir),
               "instructor solution 'sol.qmd' failed in chunk 'a'")
})

test_that("the reference is cached: the solution runs once for many submissions", {
  clear_reference_cache()
  grade_submission(sol_path())
  n <- length(ls(.reference_cache))
  grade_submission(file.path(mock_dir(), "a1_wrong_mean.qmd"))
  expect_equal(length(ls(.reference_cache)), n)
  expect_equal(n, 1)
})

test_that("print shows the score table and the error chunk", {
  r <- grade_submission(file.path(mock_dir(), "a1_error_in_chunk.qmd"))
  out <- paste(capture.output(print(r)), collapse = "\n")
  expect_match(out, "a1_error_in_chunk")
  expect_match(out, "Score: 30")
  expect_match(out, "median-by-site")
})
