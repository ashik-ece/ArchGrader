# One command that regenerates results/summary.md from scratch.
#
#   Rscript data-raw/make_summary.R                 # full run (about 3 minutes)
#   Rscript data-raw/make_summary.R --no-coverage   # skip covr, for a quick look
#   Rscript data-raw/make_summary.R --no-reports    # skip report rendering/timing
#
# It grades all 30 mock submissions, compares every criterion with the
# hand-derived answer key (inst/mock_submissions/expected_scores.csv), times
# grading with and without the feedback report, reads the hand-grading
# stopwatch times if they have been recorded, measures test coverage with covr,
# and writes:
#
#   results/summary.md       the numbers quoted in the README
#   results/mock_scores.csv  one row per mock x criterion: expected, graded
#
# Exit status is 1 if any score differs from the key or coverage is below 80%,
# so CI can use this script as the regression gate.

args <- commandArgs(trailingOnly = TRUE)
do_coverage <- !"--no-coverage" %in% args
do_reports <- !"--no-reports" %in% args
COVERAGE_GATE <- 80
AGREEMENT_GATE <- 90

suppressMessages(devtools::load_all(quiet = TRUE))

root <- file.path("inst", "mock_submissions")
key <- read.csv(file.path(root, "expected_scores.csv"), stringsAsFactors = FALSE)
planted <- read.csv(file.path(root, "planted_mistakes.csv"), stringsAsFactors = FALSE,
                    na.strings = character())
assignments <- sort(unique(key$assignment))

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
pct <- function(a, b) sprintf("%d of %d (%.1f%%)", a, b, 100 * a / b)
md_table <- function(df) {
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  rule <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  body <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(header, rule, body)
}

# --- 1. grade every mock ---------------------------------------------------
message("Grading the mock submissions ...")
out_dir <- tempfile("summary-")
rows <- list()
grade_wall <- numeric()
error_reported <- list()
for (a in assignments) {
  t0 <- Sys.time()
  b <- grade_folder(file.path(root, a), assignment = a, output_dir = file.path(out_dir, a),
                    reports = FALSE, quiet = TRUE)
  grade_wall[a] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  for (res in b$results) {
    exp <- key[key$assignment == a & key$submission == res$student_id, ]
    tab <- res$table[res$table$status != "needs_review", ]
    m <- match(tab$id, exp$criterion)
    rows[[length(rows) + 1]] <- data.frame(
      assignment = a, submission = res$student_id, criterion = tab$id,
      expected = exp$expected_points[m], graded = tab$earned,
      seconds = res$seconds, stringsAsFactors = FALSE)
    error_reported[[res$student_id]] <- res$run$error_chunk %||% ""
  }
}
cmp <- do.call(rbind, rows)
cmp$agree <- !is.na(cmp$expected) & abs(cmp$expected - cmp$graded) < 0.005
dir.create("results", showWarnings = FALSE)
write.csv(cmp[, c("assignment", "submission", "criterion", "expected", "graded", "agree")],
          file.path("results", "mock_scores.csv"), row.names = FALSE)

tot <- aggregate(cbind(expected, graded) ~ assignment + submission, cmp, sum)
tot$agree <- abs(tot$expected - tot$graded) < 0.005
tot <- merge(tot, planted[, c("submission", "kind", "error_chunk")], by = "submission")
secs <- unique(cmp[, c("assignment", "submission", "seconds")])
tot <- merge(tot, secs, by = c("assignment", "submission"))
tot <- tot[order(tot$assignment, tot$submission), ]

ptr <- tot[nzchar(tot$error_chunk), ]
pointer_ok <- vapply(seq_len(nrow(ptr)), function(i) identical(error_reported[[ptr$submission[i]]], ptr$error_chunk[i]), TRUE)

# --- 2. criteria automated -----------------------------------------------------
rubrics <- lapply(assignments, function(a)
  load_rubric(system.file("rubrics", paste0(a, ".yaml"), package = "ArchGrader")))
crit <- do.call(rbind, lapply(rubrics, function(r) data.frame(
  assignment = r$assignment,
  id = vapply(r$criteria, function(cr) cr$id, ""),
  check = vapply(r$criteria, function(cr) cr$check, ""),
  points = vapply(r$criteria, function(cr) as.numeric(cr$points), 0),
  stringsAsFactors = FALSE)))
crit$automated <- crit$check != "manual"
by_check <- as.data.frame(table(check = crit$check), stringsAsFactors = FALSE)

# --- 3. timing -------------------------------------------------------------------
report_secs <- NULL
if (do_reports && quarto_available()) {
  message("Timing report rendering (this is the slow part) ...")
  report_secs <- vapply(assignments, function(a) {
    t0 <- Sys.time()
    grade_folder(file.path(root, a), assignment = a,
                 output_dir = file.path(out_dir, paste0(a, "-reports")), reports = TRUE, quiet = TRUE)
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  }, 0)
}
n_per <- table(tot$assignment)
timing <- data.frame(
  Assignment = assignments,
  Mocks = as.integer(n_per[assignments]),
  `Median grading (s)` = vapply(assignments, function(a) fmt(median(tot$seconds[tot$assignment == a])), ""),
  `Slowest (s)` = vapply(assignments, function(a) fmt(max(tot$seconds[tot$assignment == a])), ""),
  `Grade + report, per submission (s)` = if (is.null(report_secs)) rep("not measured", length(assignments))
    else vapply(assignments, function(a) fmt(report_secs[[a]] / n_per[[a]]), ""),
  check.names = FALSE, stringsAsFactors = FALSE)

hand_path <- file.path("results", "hand_grading_times.csv")
hand <- if (file.exists(hand_path)) read.csv(hand_path, stringsAsFactors = FALSE) else NULL
hand_done <- if (is.null(hand)) NULL else hand[!is.na(hand$minutes), ]

# --- 4. coverage ------------------------------------------------------------------
cov_total <- NA_real_
cov_files <- NULL
if (do_coverage) {
  message("Measuring test coverage with covr ...")
  cov <- covr::package_coverage(quiet = TRUE)
  cov_total <- covr::percent_coverage(cov)
  lines <- covr::tally_coverage(cov, by = "line")
  per_file <- aggregate(value ~ filename, lines, function(v) 100 * mean(v > 0))
  per_file <- per_file[order(per_file$filename), ]
  cov_files <- data.frame(File = per_file$filename, Coverage = paste0(fmt(per_file$value, 1), "%"),
                          stringsAsFactors = FALSE)
}

# --- 5. write summary.md ------------------------------------------------------------
n_bad <- sum(!cmp$agree)
kinds <- c(correct = "Correct submissions scored full marks",
           alternate_valid = "Valid alternate methods accepted in full",
           mistake = "Planted mistakes scored exactly as predicted")
sha <- tryCatch(trimws(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
                error = function(e) "", warning = function(w) "")
dirty <- tryCatch(length(system2("git", c("status", "--porcelain", "--", "R", "inst", "data-raw"),
                                 stdout = TRUE, stderr = FALSE)) > 0, error = function(e) FALSE, warning = function(w) FALSE)
sha256 <- tryCatch(trimws(strsplit(system2("shasum", c("-a", "256", file.path(root, "expected_scores.csv")),
                                           stdout = TRUE), " ")[[1]][1]), error = function(e) "")

md <- c(
  "# Results summary",
  "",
  sprintf("Generated by `data-raw/make_summary.R` on %s%s (%s, %s).",
          format(Sys.Date()), if (length(sha) && nzchar(sha[1])) paste0(", commit ", sha[1], if (dirty) " plus uncommitted changes" else "") else "",
          R.version.string, R.version$platform),
  "Do not edit by hand; rerun the script.",
  "",
  "## Agreement with the hand-derived answer key",
  "",
  "The expected points for every criterion of every mock were written by hand",
  "before the grader was run (`inst/mock_submissions/expected_scores.csv`,",
  if (nzchar(sha256)) sprintf("SHA-256 `%s`).", sha256) else ").",
  "",
  md_table(data.frame(
    Measure = c("Mock submissions", "Submission totals matching the key",
                "Criterion-level scores matching the key",
                "Planted code errors reported at the right chunk",
                vapply(names(kinds), function(k) kinds[[k]], "")),
    Result = c(as.character(nrow(tot)), pct(sum(tot$agree), nrow(tot)),
               pct(sum(cmp$agree), nrow(cmp)),
               pct(sum(pointer_ok), length(pointer_ok)),
               vapply(names(kinds), function(k) pct(sum(tot$agree[tot$kind == k]), sum(tot$kind == k)), "")),
    stringsAsFactors = FALSE)),
  "",
  sprintf("Exit gate: at least %d%% criterion-level agreement. **%s** (%.1f%%).",
          AGREEMENT_GATE, if (100 * mean(cmp$agree) >= AGREEMENT_GATE) "Met" else "NOT met", 100 * mean(cmp$agree)),
  "",
  "Read this with its limit in mind: the same person wrote the rubric, the mocks and",
  "the key. Agreement shows the grader implements the rubric faithfully, not that the",
  "rubric matches what another instructor would grade.",
  "",
  "### Disagreements",
  "",
  if (n_bad == 0) "None." else c(
    md_table(data.frame(Submission = cmp$submission[!cmp$agree], Criterion = cmp$criterion[!cmp$agree],
                        Expected = fmt(cmp$expected[!cmp$agree]), Graded = fmt(cmp$graded[!cmp$agree]),
                        stringsAsFactors = FALSE))),
  "",
  "## Criteria automated",
  "",
  sprintf("%d of %d rubric criteria (%.0f%% of the points) are checked by the grader; the",
          sum(crit$automated), nrow(crit), 100 * sum(crit$points[crit$automated]) / sum(crit$points)),
  "rest are `manual` and listed for the instructor.",
  "",
  md_table(data.frame(
    Assignment = assignments,
    Criteria = vapply(assignments, function(a) as.character(sum(crit$assignment == a)), ""),
    Automated = vapply(assignments, function(a) as.character(sum(crit$assignment == a & crit$automated)), ""),
    `Points automated` = vapply(assignments, function(a)
      sprintf("%g of %g", sum(crit$points[crit$assignment == a & crit$automated]), sum(crit$points[crit$assignment == a])), ""),
    check.names = FALSE, stringsAsFactors = FALSE)),
  "",
  paste0("Check types used: ", paste(sprintf("`%s` x %d", by_check$check, by_check$Freq), collapse = ", "), "."),
  "",
  "## Time per submission",
  "",
  md_table(timing),
  "",
  "Grading time includes running the student's code in a clean process; the instructor's",
  "solution is run once per assignment and cached. The report column adds rendering the",
  "Quarto feedback report. Times depend on the machine.",
  "",
  "### Against grading by hand",
  "",
  if (is.null(hand_done) || nrow(hand_done) == 0) c(
    "Not yet recorded. Time yourself grading the ten submissions listed in",
    "`results/hand_grading_times.csv` (protocol in `results/README.md`), fill in the `minutes`",
    "column, and rerun this script."
  ) else {
    g <- merge(hand_done, secs, by = c("assignment", "submission"))
    hand_min <- mean(g$minutes)
    auto_min <- mean(g$seconds) / 60
    c(sprintf("Hand grading, %d submissions timed with a stopwatch: **%s minutes** each on average.",
              nrow(g), fmt(hand_min, 1)),
      sprintf("ArchGrader on the same submissions: **%s seconds** each (grading only).", fmt(mean(g$seconds))),
      sprintf("That is about %.0f times faster; at hand-grading speed, 100 submissions take %.1f hours, and %s with ArchGrader.",
              hand_min * 60 / max(mean(g$seconds), 0.01), hand_min * 100 / 60,
              sprintf("%.1f minutes", mean(g$seconds) * 100 / 60)))
  },
  "",
  "## Test coverage",
  "",
  if (!do_coverage) "Not measured in this run (`--no-coverage`)." else c(
    sprintf("**%s%%** of lines in `R/` are run by the test suite. Gate: %d%%. **%s**.",
            fmt(cov_total, 1), COVERAGE_GATE, if (cov_total >= COVERAGE_GATE) "Met" else "NOT met"),
    "",
    md_table(cov_files)),
  "",
  "## Every mock",
  "",
  md_table(data.frame(
    Submission = tot$submission, Kind = sub("_", " ", tot$kind), Expected = fmt(tot$expected),
    Graded = fmt(tot$graded), Match = ifelse(tot$agree, "yes", "NO"), `Seconds` = fmt(tot$seconds),
    check.names = FALSE, stringsAsFactors = FALSE)),
  ""
)
writeLines(unlist(md), file.path("results", "summary.md"))
message("Wrote results/summary.md and results/mock_scores.csv")

# --- 6. badges (self-contained SVG, so they render in a private repo too) ---------
badge_svg <- function(label, value, colour) {
  w1 <- round(6.2 * nchar(label) + 12)
  w2 <- round(6.2 * nchar(value) + 12)
  sprintf(paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="20" role="img" aria-label="%s: %s">',
    '<title>%s: %s</title>',
    '<rect width="%d" height="20" rx="3" fill="#555"/>',
    '<rect x="%d" width="%d" height="20" rx="3" fill="%s"/>',
    '<rect x="%d" width="4" height="20" fill="%s"/>',
    '<g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,sans-serif" font-size="11">',
    '<text x="%.1f" y="14">%s</text><text x="%.1f" y="14">%s</text></g></svg>'),
    w1 + w2, label, value, label, value, w1 + w2, w1, w2, colour, w1, colour,
    w1 / 2, label, w1 + w2 / 2, value)
}
badge_colour <- function(x, good, ok) if (x >= good) "#2e7d32" else if (x >= ok) "#b7791f" else "#c62828"
writeLines(badge_svg("mock agreement", sprintf("%.0f%%", 100 * mean(cmp$agree)),
                     badge_colour(100 * mean(cmp$agree), 100, AGREEMENT_GATE)),
           file.path("results", "badge-agreement.svg"))
if (do_coverage) {
  writeLines(badge_svg("coverage", sprintf("%.0f%%", cov_total), badge_colour(cov_total, 90, COVERAGE_GATE)),
             file.path("results", "badge-coverage.svg"))
}

# --- 7. console + exit status --------------------------------------------------
cat(sprintf("Totals matching: %s\n", pct(sum(tot$agree), nrow(tot))))
cat(sprintf("Criteria matching: %s\n", pct(sum(cmp$agree), nrow(cmp))))
cat(sprintf("Error pointers: %s\n", pct(sum(pointer_ok), length(pointer_ok))))
if (do_coverage) cat(sprintf("Coverage: %s%%\n", fmt(cov_total, 1)))

failed <- character()
if (n_bad > 0) failed <- c(failed, sprintf("%d criterion score(s) differ from the answer key", n_bad))
if (!all(pointer_ok)) failed <- c(failed, "an error was reported at the wrong chunk")
if (do_coverage && cov_total < COVERAGE_GATE) failed <- c(failed, sprintf("coverage %.1f%% is below %d%%", cov_total, COVERAGE_GATE))
if (length(failed)) {
  message("FAILED: ", paste(failed, collapse = "; "))
  quit(status = 1)
}
