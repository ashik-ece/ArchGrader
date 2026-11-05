score_rubric <- function(points, checks = rep("value", length(points))) {
  structure(list(criteria = Map(function(p, ch, i) {
    list(id = paste0("c", i), description = paste("Criterion", i), points = p, check = ch)
  }, points, checks, seq_along(points))), class = "archgrader_rubric")
}
res <- function(credit) check_result(credit, "note", observed = "o", expected = "e", object = "x", chunk = "k")

test_that("the score is the sum of points times credit", {
  s <- score_criteria(score_rubric(c(10, 20, 30)), list(res(1), res(0.5), res(0)))
  expect_equal(s$table$earned, c(10, 10, 0))
  expect_equal(s$earned, 20)
  expect_equal(s$possible, 60)
  expect_equal(s$percent, round(100 * 20 / 60, 2))
})

test_that("status is pass, partial or fail", {
  s <- score_criteria(score_rubric(c(10, 10, 10)), list(res(1), res(0.4), res(0)))
  expect_equal(s$table$status, c("pass", "partial", "fail"))
})

test_that("manual criteria are listed as needing review and carry no points", {
  rub <- score_rubric(c(10, 5), c("value", "manual"))
  s <- score_criteria(rub, list(res(1), check_manual(list(), NULL, NULL)))
  expect_equal(s$table$status, c("pass", "needs_review"))
  expect_true(is.na(s$table$earned[2]))
  expect_equal(s$earned, 10)
  expect_equal(s$possible, 10)
  expect_equal(s$review_points, 5)
  expect_equal(s$percent, 100)
})

test_that("points earned are rounded to two decimals, deterministically", {
  s <- score_criteria(score_rubric(25), list(res(2 / 3)))
  expect_equal(s$table$earned, 16.67)
  expect_equal(s$earned, 16.67)
})

test_that("the table carries the notes and values the report needs", {
  s <- score_criteria(score_rubric(10), list(res(1)))
  expect_named(s$table, c("id", "description", "check", "points", "credit", "earned", "status",
                          "note", "observed", "expected", "object", "chunk"))
  expect_equal(s$table$chunk, "k")
  expect_equal(s$table$observed, "o")
})

test_that("a rubric of only manual criteria does not divide by zero", {
  s <- score_criteria(score_rubric(5, "manual"), list(check_manual(list(), NULL, NULL)))
  expect_true(is.na(s$percent))
})
