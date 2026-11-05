test_that("a clean file runs and returns its objects", {
  p <- write_qmd(c(setup = "a <- 1:3", calc = "b <- sum(a)"))
  r <- run_submission(p)
  expect_null(r$error)
  expect_null(r$error_chunk)
  expect_false(r$timed_out)
  expect_equal(r$objects$b, 6L)
  expect_equal(r$chunks, c("setup", "calc"))
  expect_equal(unname(r$object_chunk[c("a", "b")]), c("setup", "calc"))
  expect_gte(r$elapsed, 0)
})

test_that("an error is reported with the failing chunk, and earlier objects survive", {
  p <- write_qmd(c(first = "x <- 1", second = "y <- 2\nz <- undefined_thing + 1\nw <- 3", third = "never <- TRUE"))
  r <- run_submission(p)
  expect_match(r$error, "undefined_thing")
  expect_equal(r$error_chunk, "second")
  expect_equal(r$objects$x, 1)
  expect_equal(r$objects$y, 2)
  expect_false("w" %in% names(r$objects))
  expect_false("never" %in% names(r$objects))
})

test_that("an infinite loop is stopped by the timeout and reported as such", {
  p <- write_qmd(c(setup = "a <- 1", spin = "while (TRUE) {}"))
  r <- run_submission(p, timeout = 3)
  expect_true(r$timed_out)
  expect_equal(r$error_chunk, "spin")
  expect_match(r$error, "did not finish within 3 seconds")
  expect_equal(r$objects$a, 1)
  expect_lt(r$elapsed, 10)
})

test_that("answers computed before a loop in the same chunk survive the timeout", {
  p <- write_qmd(c(work = "answer <- 42\nwhile (TRUE) {}\nafter <- 1"))
  r <- run_submission(p, timeout = 3)
  expect_true(r$timed_out)
  expect_equal(r$error_chunk, "work")
  expect_equal(r$objects$answer, 42)
  expect_equal(unname(r$object_chunk["answer"]), "work")
  expect_false("after" %in% names(r$objects))
})

test_that("objects assigned inside a failing expression before it failed are kept", {
  p <- write_qmd(c(work = "for (i in 1:3) {\n  seen <- i\n  if (i == 2) stop('stop at two')\n}"))
  r <- run_submission(p)
  expect_equal(r$objects$seen, 2L)
  expect_match(r$error, "stop at two")
})

test_that("a syntax error is attributed to its chunk", {
  p <- write_qmd(c(ok = "a <- 1", broken = "b <- (1 + "))
  r <- run_submission(p)
  expect_equal(r$error_chunk, "broken")
  expect_equal(r$objects$a, 1)
})

test_that("code that calls quit() is reported, not silently accepted", {
  p <- write_qmd(c(one = "a <- 1", two = "quit(save = 'no')"))
  r <- run_submission(p)
  expect_match(r$error, "ended unexpectedly")
  expect_equal(r$error_chunk, "two")
  expect_equal(r$objects$a, 1)
})

test_that("Quarto-style #| label lines are used as chunk labels", {
  p <- write_qmd(c("#| label: from-option\na <- 1", "#| label: second-one\nb <- a + 1\nstop('boom')"))
  r <- run_submission(p)
  expect_equal(r$chunks, c("from-option", "second-one"))
  expect_equal(r$error_chunk, "second-one")
})

test_that("#| label also works for chunks that already have options", {
  p <- write_qmd(c("#| label: tagged\na <- 1"))
  lines <- readLines(p)
  lines <- sub("^```\\{r\\}$", "```{r, echo=FALSE}", lines)
  writeLines(lines, p)
  expect_equal(run_submission(p)$chunks, "tagged")
})

test_that("unlabeled chunks get predictable names", {
  p <- write_qmd(c("a <- 1", named = "b <- 2", "c <- stop('x')"))
  r <- run_submission(p)
  expect_equal(r$chunks, c("unnamed-chunk-1", "named", "unnamed-chunk-2"))
  expect_equal(r$error_chunk, "unnamed-chunk-2")
})

test_that("eval = false chunks are not run", {
  p <- write_qmd(c(skipped = "stop('never')", real = "a <- 1"))
  lines <- readLines(p)
  lines <- sub("^```\\{r skipped\\}$", "```{r skipped, eval=FALSE}", lines)
  writeLines(lines, p)
  r <- run_submission(p)
  expect_null(r$error)
  expect_equal(r$objects$a, 1)
})

test_that("data files are available in the working directory, qmd files are not copied", {
  data_dir <- withr::local_tempdir()
  writeLines(c("v", "5", "7"), file.path(data_dir, "d.csv"))
  writeLines("not code", file.path(data_dir, "other.qmd"))
  p <- write_qmd(c(load = "d <- read.csv('d.csv')\nhas_qmd <- file.exists('other.qmd')"))
  r <- run_submission(p, data_dir = data_dir)
  expect_null(r$error)
  expect_equal(r$objects$d$v, c(5, 7))
  expect_false(r$objects$has_qmd)
})

test_that("one submission's state cannot leak into the next", {
  r1 <- run_submission(write_qmd(c(a = "leaked <- 42; set.seed(1)")))
  r2 <- run_submission(write_qmd(c(a = "found <- exists('leaked')")))
  expect_false(r2$objects$found)
})

test_that("the working directory is temporary: files written do not land in the project", {
  marker <- paste0("archgrader-marker-", Sys.getpid(), ".txt")
  p <- write_qmd(c(a = sprintf("writeLines('x', '%s')", marker)))
  run_submission(p)
  expect_false(file.exists(marker))
})

test_that("a missing submission file is an error", {
  expect_error(run_submission("nope.qmd"), "Submission file not found")
})

test_that("the extracted code is returned for code checks", {
  p <- write_qmd(c(a = "m <- t.test(c(1,2,3))"))
  expect_match(run_submission(p)$code, "t.test", fixed = TRUE)
})
