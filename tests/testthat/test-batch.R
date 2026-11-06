mock_dir <- function() system.file("mock_submissions", "a1", package = "ArchGrader")

# A folder with the four mocks, a copy of the solution, and three awkward files.
make_batch_folder <- function(awkward = TRUE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  four <- c("a1_error_in_chunk", "a1_missing_object", "a1_partial_table", "a1_wrong_mean")
  file.copy(file.path(mock_dir(), paste0(four, ".qmd")), d)
  file.copy(system.file("assignments", "a1_solution.qmd", package = "ArchGrader"),
            file.path(d, "b_solution_copy.qmd"))
  if (awkward) {
    writeBin(as.raw(c(0xff, 0xfe, 0x00, 0x80, 0x41, 0xc3, 0x28)), file.path(d, "z_garbage.qmd"))
    writeBin(charToRaw("```{r}\nx <- \"caf\xe9\"\n```\n"), file.path(d, "z_latin1.qmd"))
    file.create(file.path(d, "z_empty.qmd"))
  }
  d
}

test_that("a folder is graded into one CSV row per submission, in sorted order", {
  d <- make_batch_folder(awkward = FALSE)
  out <- withr::local_tempdir()
  b <- grade_folder(d, "a1", out, reports = FALSE, quiet = TRUE)
  expect_equal(nrow(b$scores), 5)
  expect_equal(b$scores$student_id,
               c("a1_error_in_chunk", "a1_missing_object", "a1_partial_table",
                 "a1_wrong_mean", "b_solution_copy"))
  csv <- read.csv(file.path(out, "scores.csv"), na.strings = "")
  expect_equal(nrow(csv), 5)
  expect_named(csv, c("student_id", "total", "renders", "mean_length", "sd_length",
                      "median_by_site", "count_by_material", "summary_table",
                      "error", "error_chunk", "seconds"))
  expect_equal(csv$total, c(30, 85, 91.67, 85, 100))
  expect_equal(csv$error, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(csv$error_chunk[1], "median-by-site")
  expect_true(all(is.na(csv$error_chunk[-1])))
})

test_that("one bad submission never stops the batch", {
  d <- make_batch_folder(awkward = TRUE)
  out <- withr::local_tempdir()
  b <- grade_folder(d, "a1", out, reports = FALSE, quiet = TRUE)
  expect_equal(nrow(b$scores), 8)
  expect_true(all(c("z_garbage", "z_latin1", "z_empty") %in% b$scores$student_id))
  # the good ones are unaffected
  expect_equal(b$scores$total[b$scores$student_id == "b_solution_copy"], 100)
})

test_that("a crashing submission still gets a report that names the chunk", {
  skip_if_not(quarto_available(), "Quarto is not available")
  d <- withr::local_tempdir()
  file.copy(file.path(mock_dir(), c("a1_error_in_chunk.qmd", "a1_wrong_mean.qmd")), d)
  out <- withr::local_tempdir()
  b <- grade_folder(d, "a1", out, quiet = TRUE)
  expect_setequal(list.files(file.path(out, "reports")), c("a1_error_in_chunk.html", "a1_wrong_mean.html"))
  html <- paste(readLines(file.path(out, "reports", "a1_error_in_chunk.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "median-by-site", fixed = TRUE)
  expect_length(b$report_paths, 2)
})

test_that("re-running a folder gives an identical CSV except the timing column", {
  d <- make_batch_folder(awkward = FALSE)
  o1 <- withr::local_tempdir()
  o2 <- withr::local_tempdir()
  grade_folder(d, "a1", o1, reports = FALSE, quiet = TRUE)
  grade_folder(d, "a1", o2, reports = FALSE, quiet = TRUE)
  a <- read.csv(file.path(o1, "scores.csv"))
  b <- read.csv(file.path(o2, "scores.csv"))
  expect_identical(a[, names(a) != "seconds"], b[, names(b) != "seconds"])
})

test_that("the console summary reports counts, average and time", {
  d <- make_batch_folder(awkward = FALSE)
  out <- withr::local_tempdir()
  text <- paste(capture.output(b <- grade_folder(d, "a1", out, reports = FALSE)), collapse = "\n")
  expect_match(text, "Graded:\\s+5 submissions")
  expect_match(text, "Average score:\\s+78\\.33 / 100 \\(78\\.3%\\)")
  expect_match(text, "With errors:\\s+1")
  expect_match(text, "Total time:")
  expect_equal(b$summary$n_errors, 1)
  expect_equal(b$summary$average_total, 78.33)
})

test_that("quiet suppresses the summary", {
  d <- make_batch_folder(awkward = FALSE)
  out <- capture.output(grade_folder(d, "a1", withr::local_tempdir(), reports = FALSE, quiet = TRUE))
  expect_length(out, 0)
})

test_that("instructor-side problems stop the batch before any student is graded", {
  d <- make_batch_folder(awkward = FALSE)
  out <- file.path(withr::local_tempdir(), "never-created")
  bad <- rubric_list()
  bad$criteria[[1]]$check <- "valeu"
  expect_error(grade_folder(d, "a1", out, reports = FALSE, rubric = write_rubric(bad)), "unknown check type")
  expect_false(dir.exists(out))
})

test_that("a missing folder, an empty folder and missing Quarto are clear errors", {
  expect_error(grade_folder("nope", reports = FALSE), "Folder not found")
  expect_error(grade_folder(withr::local_tempdir(), reports = FALSE), "No .qmd files found")
  testthat::local_mocked_bindings(quarto_available = function() FALSE)
  expect_error(grade_folder(make_batch_folder(FALSE), "a1", withr::local_tempdir()), "Quarto was not found")
})

test_that("a failing report is a warning, not a stopped batch", {
  d <- withr::local_tempdir()
  file.copy(file.path(mock_dir(), c("a1_wrong_mean.qmd", "a1_missing_object.qmd")), d)
  testthat::local_mocked_bindings(
    quarto_available = function() TRUE,
    write_feedback = function(...) stop("renderer broke")
  )
  warns <- testthat::capture_warnings(
    b <- grade_folder(d, "a1", withr::local_tempdir(), quiet = TRUE)
  )
  expect_length(warns, 2)
  expect_match(warns, "Report for a1_.* failed: renderer broke", all = TRUE)
  expect_equal(nrow(b$scores), 2)
  expect_setequal(b$summary$report_failures, c("a1_missing_object", "a1_wrong_mean"))
})
