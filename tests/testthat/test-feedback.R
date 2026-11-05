mock_path <- function(name) {
  system.file("mock_submissions", "a1", paste0(name, ".qmd"), package = "ArchGrader")
}
skip_if_no_quarto <- function() testthat::skip_if_not(quarto_available(), "Quarto is not available")

# --- escaping --------------------------------------------------------------

test_that("md_escape neutralises markdown and HTML in student text", {
  evil <- "<script>alert(1)</script> *bold* [x](http://evil) $m$ `tick` # head | pipe"
  out <- md_escape(evil)
  # every punctuation character is backslash-escaped, so nothing is live
  expect_false(grepl("(?<!\\\\)[<>*`$#|\\[]", out, perl = TRUE))
  expect_match(out, "\\\\<script\\\\>", fixed = FALSE)
})

test_that("md_escape flattens line breaks so a value cannot start a new block", {
  expect_equal(md_escape("a\n\n# heading"), "a \\# heading")
})

test_that("code_safe removes backticks and whitespace runs", {
  expect_equal(code_safe("a`b\n  c"), "a'b c")
})

test_that("fenced_block uses a fence longer than any backtick run inside", {
  blk <- fenced_block("x ```` y")
  expect_match(blk, "^`{5}\n")
  expect_match(blk, "\n`{5}$")
  expect_match(fenced_block("plain"), "^```\nplain\n```$")
})

test_that("fill_text fills placeholders, escaping outside code and sanitising inside", {
  vals <- list(object = "m", observed = "a*b", expected = "5", chunk = "my`chunk")
  expect_equal(fill_text("Got {observed}, want {expected}.", vals), "Got a\\*b, want 5.")
  expect_equal(fill_text("Check chunk `{chunk}` now", vals), "Check chunk `my'chunk` now")
})

test_that("fill_text does not expand placeholders hidden in a value", {
  vals <- list(object = "x", observed = "{chunk}", expected = "", chunk = "REAL")
  out <- fill_text("saw {observed} in {chunk}", vals)
  expect_false(grepl("saw REAL", out))
  expect_match(out, "in REAL")
})

test_that("fill_text treats missing values as empty", {
  expect_equal(fill_text("[{object}]", list(object = NA_character_)), "[]")
  expect_equal(fill_text("[{chunk}]", list()), "[]")
})

test_that("escape_mixed keeps code spans and escapes the rest", {
  expect_equal(escape_mixed("missing column(s): `n`."), "missing column\\(s\\)\\: `n`\\.")
})

# --- model -----------------------------------------------------------------

test_that("the model for a fully correct submission is clean", {
  m <- build_feedback_model(grade_submission(system.file("assignments", "a1_solution.qmd", package = "ArchGrader")))
  expect_null(m$error)
  expect_equal(m$earned, "100")
  expect_equal(m$possible, "100")
  expect_match(m$summary, "100 of 100 points")
  expect_match(m$summary, "6 of 6 scored criteria are fully correct")
  expect_length(m$blocks, 6)
  expect_true(all(vapply(m$blocks, function(b) is.null(b$hint), logical(1))))
  expect_equal(m$review, list())
})

test_that("a partial table shows details and a hint, and omits a duplicate chunk line", {
  m <- build_feedback_model(grade_submission(mock_path("a1_partial_table")))
  b <- m$blocks[[6]]
  expect_equal(b$status, "partial")
  expect_equal(b$points, "16.67 / 25")
  expect_match(b$details, "8 of 12 values match")
  expect_match(b$hint, "Build one table")
  # the rubric hint already says "Check chunk {chunk}", so no extra line
  expect_null(b$where)
  expect_match(m$summary, "91.67 of 100 points")
})

test_that("a scalar failure does not repeat observed/expected as separate details", {
  m <- build_feedback_model(grade_submission(mock_path("a1_wrong_mean")))
  b <- m$blocks[[2]]
  expect_equal(b$status, "fail")
  expect_match(b$comment, "39\\\\\\.15")   # observed value is in the comment
  expect_null(b$details)
})

test_that("an error produces an error section naming the chunk", {
  m <- build_feedback_model(grade_submission(mock_path("a1_error_in_chunk")))
  expect_equal(m$error$chunk, "median-by-site")
  expect_match(m$error$text, "chunk `median-by-site`")
  expect_match(m$error$message, "lenght_mm")
  expect_false(m$error$timed_out)
  expect_match(m$blocks[[1]]$comment, "median-by-site")
})

test_that("a timeout gets its own wording", {
  p <- write_qmd(c("overall-stats" = "mean_length <- 1\nwhile (TRUE) {}"))
  m <- build_feedback_model(grade_submission(p, timeout = 3))
  expect_true(m$error$timed_out)
  expect_match(m$error$title, "did not finish")
  expect_match(m$error$text, "time limit")
})

test_that("manual criteria become a 'needs TA review' list, not scored blocks", {
  r <- rubric_list(total_points = 15)
  r$criteria[[2]] <- list(id = "quality", description = "Reasoning quality", points = 5,
                          check = "manual", hint = "Read the argument for `{object}`.")
  r$criteria[[1]]$points <- 10
  r$criteria[[1]]$params$object <- "mean_length"
  p <- write_qmd(c("load-data" = "artifacts <- read.csv('artifacts.csv')",
                   "overall-stats" = "mean_length <- mean(artifacts$length_mm)"))
  d <- withr::local_tempdir()
  sol <- file.path(d, "sol.qmd")
  file.copy(system.file("assignments", "a1_solution.qmd", package = "ArchGrader"), sol)
  res <- grade_submission(p, rubric = write_rubric(r, dir = d), solution = sol)
  m <- build_feedback_model(res)
  expect_length(m$blocks, 1)
  expect_length(m$review, 1)
  expect_equal(m$review[[1]]$title, "Reasoning quality")
  expect_equal(m$review[[1]]$points, "5")
  expect_match(m$summary, "further 5 points will be marked by a TA")
  expect_match(m$table$Points[2], "TA review")
  expect_equal(m$table$Result[2], "⚑ Needs TA review")
})

# --- rendering -------------------------------------------------------------

test_that("write_feedback writes a self-contained report named by student id", {
  skip_if_no_quarto()
  out <- withr::local_tempdir()
  res <- grade_submission(mock_path("a1_partial_table"))
  path <- write_feedback(res, out)
  expect_equal(basename(path), "a1_partial_table.html")
  expect_equal(list.files(out), "a1_partial_table.html")
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "91.67 / 100", fixed = TRUE)
  expect_match(html, "Summary table by site", fixed = TRUE)
  expect_match(html, "8 of 12 values match", fixed = TRUE)
  expect_match(html, "Hint:", fixed = TRUE)
})

test_that("the report for an erroring submission names the failing chunk", {
  skip_if_no_quarto()
  out <- withr::local_tempdir()
  path <- write_feedback(grade_submission(mock_path("a1_error_in_chunk")), out)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "median-by-site", fixed = TRUE)
  expect_match(html, "lenght_mm", fixed = TRUE)
  expect_match(html, "Your code stopped with an error", fixed = TRUE)
})

test_that("student text cannot inject HTML into a report", {
  skip_if_no_quarto()
  d <- withr::local_tempdir()
  # hostile error message, chunk label and file name
  p <- write_qmd(c("overall-stats" = "stop('<script>alert(1)</script> **x** ```` ![i](http://e/x.png)')"),
                 dir = d, name = "a<img src=x onerror=alert(2)>.qmd")
  res <- grade_submission(p)
  out <- file.path(d, "out")
  path <- write_feedback(res, out)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("<script>alert(1)", html, fixed = TRUE))
  expect_false(grepl("<img src=x onerror", html, fixed = TRUE))
  expect_false(grepl("<img src=\"http://e/x.png", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
})

test_that("markdown output is written on request", {
  skip_if_no_quarto()
  out <- withr::local_tempdir()
  paths <- write_feedback(grade_submission(mock_path("a1_wrong_mean")), out, markdown = TRUE)
  expect_setequal(basename(paths), c("a1_wrong_mean.html", "a1_wrong_mean.md"))
  md <- paste(readLines(file.path(out, "a1_wrong_mean.md"), warn = FALSE), collapse = "\n")
  expect_match(md, "85 / 100", fixed = TRUE)
})

test_that("write_feedback validates its input and Quarto availability", {
  expect_error(write_feedback(list()), "must come from grade_submission")
  res <- grade_submission(mock_path("a1_wrong_mean"))
  testthat::local_mocked_bindings(quarto_available = function() FALSE)
  expect_error(write_feedback(res, withr::local_tempdir()), "Quarto was not found")
})

test_that("a file that cannot be run still gets a graded result and a report model", {
  testthat::local_mocked_bindings(run_submission = function(...) stop("disk exploded"))
  res <- grade_submission(mock_path("a1_wrong_mean"))
  expect_true(res$error)
  expect_equal(res$earned, 0)
  expect_match(res$run$error, "could not be read: disk exploded")
  m <- build_feedback_model(res)
  expect_match(m$error$text, "stopped with an error")
})
