#' Combine check credits and rubric points into a score table
#'
#' Score = sum of points times credit. Criteria of type `manual` get no
#' credit and no points; they are listed with status `needs_review` and their
#' points are reported separately as `review_points`. Points earned are
#' rounded to 2 decimals, so the same inputs always give the same total.
#'
#' @param rubric A rubric from `load_rubric()`.
#' @param results A list of check results (see `check_result()`), one per
#'   criterion, in rubric order.
#' @return A list with `table` (one row per criterion), `earned`, `possible`
#'   (points that can be scored automatically), `review_points` and `percent`.
#' @noRd
score_criteria <- function(rubric, results) {
  crit <- rubric$criteria
  stopifnot(length(crit) == length(results))

  chr <- function(f) vapply(results, function(r) as.character(r[[f]] %||% NA_character_), "")
  points <- vapply(crit, function(cr) as.numeric(cr$points), numeric(1))
  credit <- vapply(results, function(r) as.numeric(r$credit), numeric(1))
  manual <- vapply(crit, function(cr) identical(cr$check, "manual"), logical(1))

  earned <- ifelse(manual, NA_real_, round(points * credit, 2))
  status <- ifelse(manual, "needs_review",
            ifelse(credit >= 1 - 1e-9, "pass",
            ifelse(credit <= 1e-9, "fail", "partial")))

  tab <- data.frame(
    id = vapply(crit, function(cr) cr$id, ""),
    description = vapply(crit, function(cr) cr$description, ""),
    check = vapply(crit, function(cr) cr$check, ""),
    points = points,
    credit = ifelse(manual, NA_real_, round(credit, 4)),
    earned = earned,
    status = status,
    note = chr("note"),
    observed = chr("observed"),
    expected = chr("expected"),
    object = chr("object"),
    chunk = chr("chunk"),
    stringsAsFactors = FALSE
  )

  possible <- sum(points[!manual])
  total <- round(sum(earned, na.rm = TRUE), 2)
  list(
    table = tab,
    earned = total,
    possible = possible,
    review_points = sum(points[manual]),
    percent = if (possible > 0) round(100 * total / possible, 2) else NA_real_
  )
}
