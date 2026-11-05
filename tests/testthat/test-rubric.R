test_that("the packaged Assignment 1 rubric loads", {
  path <- system.file("rubrics", "a1.yaml", package = "ArchGrader")
  rub <- load_rubric(path)
  expect_s3_class(rub, "archgrader_rubric")
  expect_equal(rub$assignment, "a1")
  expect_gte(length(rub$criteria), 6)
  expect_lte(length(rub$criteria), 8)
  expect_equal(sum(vapply(rub$criteria, function(cr) cr$points, numeric(1))), rub$total_points)
})

test_that("a valid rubric loads and criteria get empty params by default", {
  r <- rubric_list()
  r$criteria[[1]] <- list(
    id = "r", description = "Runs", points = 10, check = "renders",
    comments = list(pass = "a", partial = "b", fail = "c"), hint = "h"
  )
  rub <- load_rubric(write_rubric(r))
  expect_equal(rub$criteria[[1]]$params, list())
})

test_that("a typo in a check type fails on load with a readable error", {
  r <- rubric_list()
  r$criteria[[1]]$check <- "valeu"
  expect_error(load_rubric(write_rubric(r)), "unknown check type 'valeu'")
  expect_error(load_rubric(write_rubric(r)), "Valid types: renders, value, plot, uses_function, text_mentions, manual")
})

test_that("unknown fields are rejected at both levels", {
  r <- rubric_list(extra_field = 1)
  expect_error(load_rubric(write_rubric(r)), "unknown top-level field\\(s\\): extra_field")
  r <- rubric_list()
  r$criteria[[1]]$pionts <- 3
  expect_error(load_rubric(write_rubric(r)), "unknown field\\(s\\): pionts")
})

test_that("required fields must be present", {
  r <- rubric_list()
  r$criteria[[1]]$hint <- NULL
  expect_error(load_rubric(write_rubric(r)), "missing field 'hint'")
  r <- rubric_list()
  r$title <- NULL
  expect_error(load_rubric(write_rubric(r)), "missing top-level field\\(s\\): title")
  # present but empty is also rejected
  r$title <- ""
  expect_error(load_rubric(write_rubric(r)), "'title' must be a non-empty text value")
})

test_that("points must be positive and add up to total_points", {
  r <- rubric_list()
  r$criteria[[1]]$points <- -5
  expect_error(load_rubric(write_rubric(r)), "'points' must be a positive number")
  r <- rubric_list(total_points = 12)
  expect_error(load_rubric(write_rubric(r)), "add up to 10 but total_points is 12")
})

test_that("criterion ids must be unique", {
  r <- rubric_list(total_points = 20)
  r$criteria[[2]] <- r$criteria[[1]]
  expect_error(load_rubric(write_rubric(r)), "duplicate criterion id\\(s\\): x")
})

test_that("criterion ids cannot clash with scores.csv columns", {
  for (id in c("total", "seconds", "error", "student_id", "error_chunk")) {
    r <- rubric_list()
    r$criteria[[1]]$id <- id
    expect_error(load_rubric(write_rubric(r)), paste0("'id' cannot be '", id, "'"))
  }
})

test_that("check parameters are validated", {
  r <- rubric_list()
  r$criteria[[1]]$params$tolerence <- 1
  expect_error(load_rubric(write_rubric(r)), "unknown parameter\\(s\\) for check 'value': tolerence")
  r <- rubric_list()
  r$criteria[[1]]$params$tolerance <- NULL
  expect_error(load_rubric(write_rubric(r)), "needs parameter\\(s\\): tolerance")
  r <- rubric_list()
  r$criteria[[1]]$params$tolerance <- -1
  expect_error(load_rubric(write_rubric(r)), "'tolerance' must be a number of 0 or more")
})

test_that("comments need pass, partial and fail, and only known placeholders", {
  r <- rubric_list()
  r$criteria[[1]]$comments$partial <- NULL
  expect_error(load_rubric(write_rubric(r)), "'comments' is missing: partial")
  r <- rubric_list()
  r$criteria[[1]]$comments$pass <- "value was {obseved}"
  expect_error(load_rubric(write_rubric(r)), "unknown placeholder\\(s\\): \\{obseved\\}")
})

test_that("manual criteria do not need comments or a hint", {
  r <- rubric_list()
  r$criteria[[1]] <- list(id = "m", description = "Quality", points = 10, check = "manual")
  expect_s3_class(load_rubric(write_rubric(r)), "archgrader_rubric")
})

test_that("all problems are reported together", {
  r <- rubric_list(total_points = 99)
  r$criteria[[1]]$check <- "nope"
  r$criteria[[1]]$points <- 0
  err <- tryCatch(load_rubric(write_rubric(r)), error = conditionMessage)
  expect_match(err, "unknown check type")
  expect_match(err, "'points' must be a positive number")
})

test_that("missing or malformed files fail clearly", {
  expect_error(load_rubric("does-not-exist.yaml"), "Rubric file not found")
  bad <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c("criteria: [unclosed", "  - x: : :"), bad)
  expect_error(load_rubric(bad), "not valid YAML")
})
