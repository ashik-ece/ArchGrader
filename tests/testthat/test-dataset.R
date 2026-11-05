artifacts_path <- function() {
  # Works under devtools::test() and an installed package
  p <- system.file("assignments", "artifacts.csv", package = "ArchGrader")
  if (!nzchar(p)) p <- testthat::test_path("..", "..", "inst", "assignments", "artifacts.csv")
  p
}

test_that("the synthetic dataset has the agreed shape", {
  a <- read.csv(artifacts_path())
  expect_equal(nrow(a), 150)
  expect_named(a, c("artifact_id", "site", "raw_material",
                    "length_mm", "width_mm", "thickness_mm"))
  expect_false(anyNA(a))
  expect_equal(anyDuplicated(a$artifact_id), 0)
  expect_setequal(a$site, c("Ridge Camp", "River Terrace", "Cave Mouth", "Hilltop"))
  expect_setequal(a$raw_material, c("chert", "obsidian", "quartzite", "basalt"))
  expect_true(all(a$length_mm > 0 & a$width_mm > 0 & a$thickness_mm > 0))
})
