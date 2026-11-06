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

test_that("bare y, n, yes, no, on and off stay text, only true/false are booleans", {
  r <- rubric_list()
  r$criteria[[1]] <- list(
    id = "p", description = "Plot", points = 10, check = "plot",
    params = list(object = "p", mapping = list(x = "len", y = "wid"), labels = c("x", "y")),
    comments = list(pass = "a", partial = "b", fail = "c"), hint = "h")
  path <- write_rubric(r)
  # write_yaml quotes "y", so write the risky form by hand
  txt <- readLines(path)
  txt <- gsub("'y'", "y", gsub("\"y\"", "y", txt, fixed = TRUE), fixed = TRUE)
  writeLines(txt, path)
  expect_true(any(grepl("(^|\\s)y(:|$)", readLines(path))))
  rub <- load_rubric(path)
  p <- rub$criteria[[1]]$params
  expect_named(p$mapping, c("x", "y"))
  expect_equal(unlist(p$labels), c("x", "y"))
})

test_that("only true and false become booleans in a rubric", {
  yml <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "assignment: t", "title: T", "solution: s.qmd", "total_points: 5",
    "criteria:",
    "  - id: tm", "    description: d", "    points: 5", "    check: text_mentions",
    "    params:", "      heading: H", "      groups:", "        yesno: [yes, no, on, off, y, n]",
    "    comments: {pass: a, partial: b, fail: c}", "    hint: h"), yml)
  rub <- load_rubric(yml)
  expect_equal(unlist(rub$criteria[[1]]$params$groups$yesno), c("yes", "no", "on", "off", "y", "n"))
  yml2 <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "assignment: t", "title: T", "solution: s.qmd", "total_points: 5",
    "criteria:",
    "  - id: v", "    description: d", "    points: 5", "    check: value",
    "    params: {object: x, tolerance: 0.1, ignore_sign: true}",
    "    comments: {pass: a, partial: b, fail: c}", "    hint: h"), yml2)
  expect_identical(load_rubric(yml2)$criteria[[1]]$params$ignore_sign, TRUE)
})

test_that("plot parameters are validated", {
  dir <- withr::local_tempdir()
  mk <- function(params) {
    r <- rubric_list()
    r$criteria[[1]] <- list(id = "p", description = "P", points = 10, check = "plot",
                            params = params,
                            comments = list(pass = "a", partial = "b", fail = "c"), hint = "h")
    write_rubric(r, dir = dir)
  }
  expect_error(load_rubric(mk(list(object = "p"))), "needs at least one of: geom, mapping, labels")
  expect_error(load_rubric(mk(list(object = "p", geom = "point", colour = "x"))), "unknown parameter\\(s\\) for check 'plot': colour")
  expect_error(load_rubric(mk(list(object = "p", labels = c("x", "ttle")))), "param 'labels' must be a list drawn from")
  expect_error(load_rubric(mk(list(object = "p", mapping = "x"))), "param 'mapping' must map aesthetic names")
  expect_s3_class(load_rubric(mk(list(object = "p", geom = "point", mapping = list(x = "a"), labels = "title"))), "archgrader_rubric")
})

test_that("uses_function and text_mentions parameters are validated", {
  dir <- withr::local_tempdir()
  mk <- function(check, params) {
    r <- rubric_list()
    r$criteria[[1]] <- list(id = "c", description = "C", points = 10, check = check, params = params,
                            comments = list(pass = "a", partial = "b", fail = "c"), hint = "h")
    write_rubric(r, dir = dir)
  }
  expect_error(load_rubric(mk("uses_function", list())), "needs parameter\\(s\\): functions")
  expect_error(load_rubric(mk("uses_function", list(functions = "lm", mode = "some"))), "'mode' must be 'any' or 'all'")
  expect_error(load_rubric(mk("uses_function", list(functoins = "lm"))), "unknown parameter\\(s\\)")
  expect_error(load_rubric(mk("text_mentions", list(heading = "H"))), "needs parameter\\(s\\): groups")
  expect_error(load_rubric(mk("text_mentions", list(heading = "H", groups = list("a", "b")))), "must map a group name to a list of keywords")
  expect_s3_class(load_rubric(mk("text_mentions", list(heading = "H", groups = list(a = c("x", "y"))))), "archgrader_rubric")
})

test_that("all three packaged rubrics load and use only the six check types", {
  for (a in c("a1", "a2", "a3")) {
    rub <- load_rubric(system.file("rubrics", paste0(a, ".yaml"), package = "ArchGrader"))
    expect_true(all(vapply(rub$criteria, function(cr) cr$check, "") %in% CHECK_TYPES), info = a)
    expect_gte(length(rub$criteria), 6)
    expect_lte(length(rub$criteria), 8)
  }
})
