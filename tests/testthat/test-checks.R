ref_table <- data.frame(site = c("A", "B", "C"), m = c(1.5, 2.5, 3.5), n = c(10L, 20L, 30L))
ref <- fake_run(list(x = 5, tbl = ref_table, v = c(1, 2, 3), lbl = "hello"),
                object_chunk = c(x = "calc", tbl = "table-chunk", v = "vec", lbl = "lbl-chunk"))
value_params <- function(object, tolerance = 0.01, key = NULL) {
  p <- list(object = object, tolerance = tolerance)
  if (!is.null(key)) p$key <- key
  p
}

# --- check_result ----------------------------------------------------------

test_that("check_result enforces a credit between 0 and 1 (or NA)", {
  expect_error(check_result(1.2, "x"))
  expect_error(check_result(-0.1, "x"))
  expect_true(is.na(check_result(NA_real_, "x")$credit))
  expect_equal(check_result(0.5, "x")$credit, 0.5)
})

# --- renders ---------------------------------------------------------------

test_that("renders: pass and fail", {
  ok <- check_renders(list(), fake_run(), ref)
  expect_equal(ok$credit, 1)
  bad <- check_renders(list(), fake_run(error = "boom", error_chunk = "calc"), ref)
  expect_equal(bad$credit, 0)
  expect_equal(bad$chunk, "calc")
  expect_match(bad$note, "chunk `calc`")
  expect_match(bad$note, "boom")
})

test_that("renders: a timeout is described as such", {
  r <- check_renders(list(), fake_run(error = "The code did not finish within 30 seconds.",
                                      error_chunk = "spin", timed_out = TRUE), ref)
  expect_equal(r$credit, 0)
  expect_match(r$note, "stopped")
})

# --- value: numbers --------------------------------------------------------

test_that("value: a number within tolerance passes, outside fails", {
  pass <- check_value(value_params("x"), fake_run(list(x = 5.005)), ref)
  expect_equal(pass$credit, 1)
  fail <- check_value(value_params("x"), fake_run(list(x = 5.5)), ref)
  expect_equal(fail$credit, 0)
  expect_match(fail$note, "Observed 5.5; expected 5")
  expect_equal(fail$observed, "5.5")
  expect_equal(fail$expected, "5")
})

test_that("value: the tolerance boundary is inclusive", {
  expect_equal(check_value(value_params("x", 0.5), fake_run(list(x = 5.5)), ref)$credit, 1)
  expect_equal(check_value(value_params("x", 0.5), fake_run(list(x = 5.6)), ref)$credit, 0)
})

test_that("value: a missing object scores zero and names the object and the solution's chunk", {
  r <- check_value(value_params("x"), fake_run(list(other = 1)), ref)
  expect_equal(r$credit, 0)
  expect_match(r$note, "`x` was not created")
  expect_equal(r$object, "x")
  expect_equal(r$chunk, "calc")
})

test_that("value: a NULL placeholder counts as not answered", {
  r <- check_value(value_params("x"), fake_run(list(x = NULL)), ref)
  expect_equal(r$credit, 0)
  expect_match(r$note, "still NULL")
})

test_that("value: the chunk points at the student's chunk when the object exists", {
  r <- check_value(value_params("x"), fake_run(list(x = 9), object_chunk = c(x = "my-chunk")), ref)
  expect_equal(r$chunk, "my-chunk")
})

test_that("value: wrong type gets zero", {
  expect_equal(check_value(value_params("x"), fake_run(list(x = "5")), ref)$credit, 0)
  expect_equal(check_value(value_params("x"), fake_run(list(x = c(5, 5))), ref)$credit, 0)
})

test_that("value: numeric vectors give partial credit per element", {
  r <- check_value(value_params("v"), fake_run(list(v = c(1, 2, 99))), ref)
  expect_equal(r$credit, 2 / 3)
  expect_match(r$note, "2 of 3 values match")
})

test_that("value: other objects compare as text", {
  expect_equal(check_value(value_params("lbl"), fake_run(list(lbl = "hello")), ref)$credit, 1)
  expect_equal(check_value(value_params("lbl"), fake_run(list(lbl = "bye")), ref)$credit, 0)
})

# --- value: tables ---------------------------------------------------------

test_that("value table: identical table passes", {
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = ref_table)), ref)
  expect_equal(r$credit, 1)
})

test_that("value table: row order does not matter when a key is given", {
  shuffled <- ref_table[c(3, 1, 2), ]
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = shuffled)), ref)
  expect_equal(r$credit, 1)
})

test_that("value table: tibbles and factors are accepted", {
  alt <- ref_table
  alt$site <- factor(alt$site)
  class(alt) <- c("tbl_df", "tbl", "data.frame")
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = alt)), ref)
  expect_equal(r$credit, 1)
})

test_that("value table: partial credit is the share of matching cells", {
  wrong <- ref_table
  wrong$m[2] <- 99
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = wrong)), ref)
  expect_equal(r$credit, 5 / 6)
  expect_match(r$note, "5 of 6 values match")
})

test_that("value table: a missing column loses that column's cells and is named", {
  partial <- ref_table[, c("site", "m")]
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = partial)), ref)
  expect_equal(r$credit, 0.5)
  expect_match(r$note, "missing column\\(s\\): `n`")
})

test_that("value table: a missing row loses its cells", {
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = ref_table[1:2, ])), ref)
  expect_equal(r$credit, 4 / 6)
  expect_match(r$note, "expected 3 rows but got 2")
})

test_that("value table: a missing key column scores zero", {
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = ref_table[, c("m", "n")])), ref)
  expect_equal(r$credit, 0)
  expect_match(r$note, "`site`")
})

test_that("value table: not a data frame scores zero", {
  r <- check_value(value_params("tbl", key = "site"), fake_run(list(tbl = 3)), ref)
  expect_equal(r$credit, 0)
  expect_match(r$note, "Expected a data frame")
})

test_that("value table: without a key rows match by position", {
  r <- check_value(value_params("tbl"), fake_run(list(tbl = ref_table[c(2, 1, 3), ])), ref)
  expect_lt(r$credit, 1)
})

test_that("value: a rubric naming an object the solution lacks is a loud error", {
  expect_error(check_value(value_params("nope"), fake_run(list(nope = 1)), ref),
               "instructor solution does not create an object named 'nope'")
})

# --- manual, dispatch ------------------------------------------------------

test_that("manual gives no credit and asks for review", {
  r <- check_manual(list(), fake_run(), ref)
  expect_true(is.na(r$credit))
  expect_match(r$note, "TA review")
})

test_that("run_check dispatches, and unimplemented types fail loudly", {
  cr <- list(id = "a", check = "renders", params = list())
  expect_equal(run_check(cr, fake_run(), ref)$credit, 1)
  cr <- list(id = "p", check = "plot", params = list())
  expect_error(run_check(cr, fake_run(), ref), "not implemented yet")
})
