# Regression test: grade every mock submission and compare with the answer key
# (inst/mock_submissions/expected_scores.csv), which was written by hand before
# the grader was run. Any change to a check, a rubric or the runner that moves a
# score fails here.

mock_root <- function() system.file("mock_submissions", package = "ArchGrader")

test_that("the answer key is complete and well formed", {
  key <- read.csv(file.path(mock_root(), "expected_scores.csv"))
  planted <- read.csv(file.path(mock_root(), "planted_mistakes.csv"), na.strings = character())
  expect_named(key, c("assignment", "submission", "criterion", "expected_points"))
  expect_equal(length(unique(key$submission)), 30)
  expect_equal(as.vector(table(planted$assignment)), c(10, 10, 10))
  expect_setequal(planted$kind, c("correct", "mistake", "alternate_valid"))
  expect_gte(sum(planted$kind == "alternate_valid"), 3)
  # every mock in the key has a file, and every file has a key
  files <- sub("\\.qmd$", "", basename(list.files(mock_root(), pattern = "\\.qmd$", recursive = TRUE)))
  expect_setequal(files, unique(key$submission))
  expect_setequal(files, planted$submission)
})

test_that("every mock scores exactly what was written in the answer key", {
  key <- read.csv(file.path(mock_root(), "expected_scores.csv"))
  planted <- read.csv(file.path(mock_root(), "planted_mistakes.csv"), na.strings = character())
  for (a in unique(key$assignment)) {
    out <- withr::local_tempdir()
    b <- grade_folder(file.path(mock_root(), a), a, out, reports = FALSE, quiet = TRUE)
    for (sub in names(b$results)) {
      res <- b$results[[sub]]
      want <- key[key$submission == sub, ]
      tab <- res$table[res$table$status != "needs_review", ]
      got <- stats::setNames(tab$earned, tab$id)
      expected <- stats::setNames(want$expected_points, want$criterion)[names(got)]
      expect_equal(unname(got), unname(expected), info = sub)

      # a planted code error must be reported at the true failing chunk
      chunk <- planted$error_chunk[planted$submission == sub]
      if (nzchar(chunk)) expect_equal(res$run$error_chunk, chunk, info = sub) else expect_null(res$run$error_chunk, info = sub)
    }
  }
})

test_that("the alternate-method mocks are accepted in full (no false negatives)", {
  planted <- read.csv(file.path(mock_root(), "planted_mistakes.csv"), na.strings = character())
  alt <- planted[planted$kind == "alternate_valid", ]
  for (i in seq_len(nrow(alt))) {
    res <- grade_submission(file.path(mock_root(), alt$assignment[i], paste0(alt$submission[i], ".qmd")),
                            assignment = alt$assignment[i])
    scored <- res$table[res$table$status != "needs_review", ]
    expect_true(all(scored$status == "pass"), info = alt$submission[i])
    expect_equal(res$percent, 100, info = alt$submission[i])
  }
})

test_that("all three instructor solutions score 100%", {
  for (a in c("a1", "a2", "a3")) {
    res <- grade_submission(system.file("assignments", paste0(a, "_solution.qmd"), package = "ArchGrader"), assignment = a)
    expect_equal(res$percent, 100, info = a)
    expect_false(res$error, info = a)
  }
})
