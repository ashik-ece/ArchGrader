# Grades every mock submission and compares the result with the answer key.
#
# Run from the project root:   Rscript data-raw/evaluate_mocks.R
#
# Prints, for each mock, the expected and graded total, lists every
# criterion-level disagreement, and reports the headline agreement numbers.
# Phase 5 extends this to write results/summary.md.

suppressMessages(devtools::load_all(quiet = TRUE))

root <- file.path("inst", "mock_submissions")
key <- read.csv(file.path(root, "expected_scores.csv"), stringsAsFactors = FALSE)
planted <- read.csv(file.path(root, "planted_mistakes.csv"), stringsAsFactors = FALSE, na.strings = character())

out_dir <- tempfile("mock-eval-")
rows <- list()
for (a in unique(key$assignment)) {
  b <- grade_folder(file.path(root, a), assignment = a, output_dir = file.path(out_dir, a),
                    reports = FALSE, quiet = TRUE)
  for (res in b$results) {
    exp <- key[key$assignment == a & key$submission == res$student_id, ]
    tab <- res$table[res$table$status != "needs_review", ]
    m <- match(tab$id, exp$criterion)
    rows[[length(rows) + 1]] <- data.frame(
      assignment = a, submission = res$student_id, criterion = tab$id,
      expected = exp$expected_points[m], graded = tab$earned,
      seconds = res$seconds, stringsAsFactors = FALSE)
  }
}
cmp <- do.call(rbind, rows)
cmp$agree <- abs(cmp$expected - cmp$graded) < 0.005

# --- totals ----------------------------------------------------------------
tot <- aggregate(cbind(expected, graded) ~ assignment + submission, cmp, sum)
tot$agree <- abs(tot$expected - tot$graded) < 0.005
cat("\nTotals (expected vs graded)\n")
print(tot[order(tot$assignment, tot$submission), ], row.names = FALSE)

# --- disagreements -----------------------------------------------------------
bad <- cmp[!cmp$agree, c("assignment", "submission", "criterion", "expected", "graded")]
cat("\nCriterion-level disagreements:", nrow(bad), "of", nrow(cmp), "\n")
if (nrow(bad)) print(bad, row.names = FALSE)

# --- error pointers ----------------------------------------------------------
ptr <- planted[nzchar(planted$error_chunk), ]
reported <- vapply(seq_len(nrow(ptr)), function(i) {
  r <- grade_submission(file.path(root, ptr$assignment[i], paste0(ptr$submission[i], ".qmd")),
                        assignment = ptr$assignment[i])
  r$run$error_chunk %||% NA_character_
}, "")
cat("\nError pointers:", sum(reported == ptr$error_chunk, na.rm = TRUE), "of", nrow(ptr), "correct\n")
print(data.frame(submission = ptr$submission, planted = ptr$error_chunk, reported = reported), row.names = FALSE)

# --- headline numbers --------------------------------------------------------
cat("\nHeadline\n")
cat(sprintf("  Mock totals matching expected: %d of %d (%.1f%%)\n",
            sum(tot$agree), nrow(tot), 100 * mean(tot$agree)))
cat(sprintf("  Criterion-level agreement:     %d of %d (%.1f%%)\n",
            sum(cmp$agree), nrow(cmp), 100 * mean(cmp$agree)))
by_kind <- merge(tot, planted[, c("submission", "kind")], by = "submission")
for (k in unique(by_kind$kind)) {
  cat(sprintf("  %-16s totals matching: %d of %d\n", k, sum(by_kind$agree[by_kind$kind == k]), sum(by_kind$kind == k)))
}
cat(sprintf("  Median seconds per mock (grading only): %.2f\n", median(unique(cmp[, c("submission", "seconds")])$seconds)))

invisible(list(totals = tot, comparison = cmp, disagreements = bad))
