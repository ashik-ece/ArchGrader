# Generates the mock submissions (inst/mock_submissions/<assignment>/*.qmd) and
# the answer key (expected_scores.csv, planted_mistakes.csv).
#
# Run from the project root:   Rscript data-raw/mocks.R
#
# THE ANSWER KEY IS WRITTEN HERE, BY HAND, BEFORE THE GRADER SEES THE MOCKS.
# Each mock lists the points it should earn on every criterion, worked out by
# applying the rubric's rules to the planted mistake (see the comments). Do not
# edit an expected value to make a result look better. If an expected score
# turns out to be wrong, change it here and record why in
# docs/disagreement-log.md.
#
# These mocks are written by the author, so agreement with them shows that the
# checker does what the rubric says, not how it copes with real student variety.
# The `alternate_valid` mocks are correct answers reached by a different method,
# included to test that the checks do not reject valid alternatives.

out_root <- file.path("inst", "mock_submissions")

# --- helpers ---------------------------------------------------------------

template <- function(a) readLines(file.path("inst", "assignments", paste0(a, ".qmd")), warn = FALSE)

# Replace the placeholder line `<name> <- NULL ...` with code. A replacement of
# NA deletes the line (used when one block of code fills several placeholders).
fill <- function(lines, code) {
  for (nm in names(code)) {
    i <- grep(paste0("^", nm, " <- NULL"), lines)
    stopifnot(length(i) == 1)
    lines[i] <- code[[nm]]
  }
  lines[!is.na(lines)]
}

fill_interpretation <- function(lines, text) {
  i <- grep("^\\*\\(Write your interpretation here", lines)
  stopifnot(length(i) == 1)
  lines[i] <- text
  lines
}

mocks <- list()
add_mock <- function(assignment, name, kind, mistake, expected, code = list(),
                     interpretation = NULL, error_chunk = "") {
  mocks[[length(mocks) + 1]] <<- list(
    assignment = assignment, name = name, kind = kind, mistake = mistake,
    expected = expected, code = code, interpretation = interpretation,
    error_chunk = error_chunk
  )
}

# Expected points: start from full marks on every automatically scored
# criterion, then override only what the planted mistake changes.
full <- list(
  a1 = c(renders = 10, mean_length = 15, sd_length = 15, median_by_site = 20,
         count_by_material = 15, summary_table = 25),
  a2 = c(renders = 10, points_layer = 20, axes_mapped = 20, colour_mapped = 15,
         axis_labels = 15, title = 10),
  a3 = c(renders = 10, t_statistic = 10, p_value = 10, conf_int = 15,
         cohens_d = 15, method = 10, interpretation = 20)
)
expect <- function(a, ...) {
  e <- full[[a]]
  over <- c(...)
  e[names(over)] <- over
  e
}
zero <- function(a, keep = character()) {
  e <- full[[a]] * 0
  e[keep] <- full[[a]][keep]
  e
}

# ===========================================================================
# Assignment 1: summary statistics (10 mocks)
# ===========================================================================

a1_good <- list(
  mean_length = "mean_length <- mean(artifacts$length_mm)",
  sd_length = "sd_length <- sd(artifacts$length_mm)",
  median_by_site = paste(
    "median_by_site <- aggregate(length_mm ~ site, data = artifacts, FUN = median)",
    "names(median_by_site)[2] <- \"median_length\"", sep = "\n"),
  count_by_material = paste(
    "count_by_material <- as.data.frame(table(artifacts$raw_material))",
    "names(count_by_material) <- c(\"raw_material\", \"n\")", sep = "\n"),
  summary_table = paste(
    "summary_table <- data.frame(",
    "  site = sort(unique(artifacts$site)),",
    "  n = as.vector(table(artifacts$site)),",
    "  mean_length = as.vector(tapply(artifacts$length_mm, artifacts$site, mean)),",
    "  sd_length = as.vector(tapply(artifacts$length_mm, artifacts$site, sd))",
    ")", sep = "\n")
)
a1 <- function(...) utils::modifyList(a1_good, list(...))

# 1. median used instead of mean. 39.15 differs from 39.046 by 0.104 > 0.01, so
#    mean_length earns 0. Everything else is correct.
add_mock("a1", "a1_wrong_mean", "mistake",
         "mean_length computed with median() instead of mean()",
         expect("a1", mean_length = 0),
         a1(mean_length = "mean_length <- median(artifacts$length_mm)"))

# 2. count_by_material saved under another name, so the object is missing: 0.
add_mock("a1", "a1_missing_object", "mistake",
         "count_by_material saved under the wrong name (material_counts)",
         expect("a1", count_by_material = 0),
         a1(count_by_material = paste(
           "material_counts <- as.data.frame(table(artifacts$raw_material))",
           "names(material_counts) <- c(\"raw_material\", \"n\")", sep = "\n")))

# 3. Typo (lenght_mm) throws an error in median-by-site. renders earns 0.
#    mean_length and sd_length ran earlier, so they keep full marks. The three
#    later answers never run, so they are missing: 0.
add_mock("a1", "a1_error_in_chunk", "mistake",
         "typo lenght_mm in the median-by-site chunk throws an error",
         expect("a1", renders = 0, median_by_site = 0, count_by_material = 0, summary_table = 0),
         a1(median_by_site = paste(
           "median_by_site <- aggregate(lenght_mm ~ site, data = artifacts, FUN = median)",
           "names(median_by_site)[2] <- \"median_length\"", sep = "\n")),
         error_chunk = "median-by-site")

# 4. sd_length column of summary_table is the variance. The table has 4 sites x
#    3 value columns = 12 cells; n and mean_length are right (8), sd_length is
#    wrong (4). Credit 8/12, so 25 x 8/12 = 16.67.
add_mock("a1", "a1_partial_table", "mistake",
         "sd_length column of summary_table computed with var() instead of sd()",
         expect("a1", summary_table = 16.67),
         a1(summary_table = paste(
           "summary_table <- data.frame(",
           "  site = sort(unique(artifacts$site)),",
           "  n = as.vector(table(artifacts$site)),",
           "  mean_length = as.vector(tapply(artifacts$length_mm, artifacts$site, mean)),",
           "  sd_length = as.vector(tapply(artifacts$length_mm, artifacts$site, var))",
           ")", sep = "\n")))

# 5. Correct, in dplyr (tibbles). Full marks.
add_mock("a1", "a1_correct_dplyr", "correct", "none (correct answer, dplyr and tibbles)",
         expect("a1"),
         a1(
           median_by_site = paste(
             "median_by_site <- artifacts |>",
             "  dplyr::group_by(site) |>",
             "  dplyr::summarise(median_length = median(length_mm))", sep = "\n"),
           count_by_material = "count_by_material <- artifacts |> dplyr::count(raw_material)",
           summary_table = paste(
             "summary_table <- artifacts |>",
             "  dplyr::group_by(site) |>",
             "  dplyr::summarise(n = dplyr::n(), mean_length = mean(length_mm), sd_length = sd(length_mm))",
             sep = "\n")))

# 6. Correct, hand-written base R (no mean(), sd(), aggregate or table helpers
#    from the instructor solution). Full marks.
add_mock("a1", "a1_alt_base_r", "alternate_valid",
         "none (correct answer by a different method: base R, formulas written out)",
         expect("a1"),
         a1(
           mean_length = "mean_length <- sum(artifacts$length_mm) / length(artifacts$length_mm)",
           sd_length = "sd_length <- sqrt(sum((artifacts$length_mm - mean_length)^2) / (length(artifacts$length_mm) - 1))",
           median_by_site = paste(
             "site_medians <- tapply(artifacts$length_mm, artifacts$site, median)",
             "median_by_site <- data.frame(site = names(site_medians), median_length = as.numeric(site_medians))",
             sep = "\n"),
           count_by_material = paste(
             "mat_counts <- table(artifacts$raw_material)",
             "count_by_material <- data.frame(raw_material = names(mat_counts), n = as.integer(mat_counts))",
             sep = "\n"),
           summary_table = paste(
             "by_site <- split(artifacts$length_mm, artifacts$site)",
             "summary_table <- data.frame(",
             "  site = names(by_site),",
             "  n = sapply(by_site, length),",
             "  mean_length = sapply(by_site, mean),",
             "  sd_length = sapply(by_site, sd)",
             ")", sep = "\n")))

# 7. Correct, but every table is sorted descending, so rows are in a different
#    order from the solution. Rows are matched on the key column, so full marks.
add_mock("a1", "a1_alt_reordered_rows", "alternate_valid",
         "none (correct answer; tables sorted in a different row order)",
         expect("a1"),
         a1(
           median_by_site = paste(
             "median_by_site <- artifacts |>",
             "  dplyr::group_by(site) |>",
             "  dplyr::summarise(median_length = median(length_mm)) |>",
             "  dplyr::arrange(dplyr::desc(median_length))", sep = "\n"),
           count_by_material = "count_by_material <- artifacts |> dplyr::count(raw_material, sort = TRUE)",
           summary_table = paste(
             "summary_table <- artifacts |>",
             "  dplyr::group_by(site) |>",
             "  dplyr::summarise(n = dplyr::n(), mean_length = mean(length_mm), sd_length = sd(length_mm)) |>",
             "  dplyr::arrange(dplyr::desc(mean_length))", sep = "\n")))

# 8. summary_table columns named mean and sd instead of mean_length and
#    sd_length. Only n (4 of 12 cells) matches by name: credit 4/12, so
#    25 x 4/12 = 8.33.
add_mock("a1", "a1_wrong_names", "mistake",
         "summary_table columns named mean and sd instead of mean_length and sd_length",
         expect("a1", summary_table = 8.33),
         a1(summary_table = paste(
           "summary_table <- data.frame(",
           "  site = sort(unique(artifacts$site)),",
           "  n = as.vector(table(artifacts$site)),",
           "  mean = as.vector(tapply(artifacts$length_mm, artifacts$site, mean)),",
           "  sd = as.vector(tapply(artifacts$length_mm, artifacts$site, sd))",
           ")", sep = "\n")))

# 9. Template handed back unchanged: it runs (10) but every object is still NULL.
add_mock("a1", "a1_template_unfilled", "mistake",
         "template submitted without writing any code (all answers still NULL)",
         zero("a1", keep = "renders"), list())

# 10. Everything rounded to 2 decimals. Every value is within the 0.01
#     tolerance of the exact answer (largest gap is 0.0049), so full marks.
add_mock("a1", "a1_rounded_two_decimals", "alternate_valid",
         "none (correct answer, values rounded to 2 decimals, within tolerance)",
         expect("a1"),
         a1(
           mean_length = "mean_length <- round(mean(artifacts$length_mm), 2)",
           sd_length = "sd_length <- round(sd(artifacts$length_mm), 2)",
           summary_table = paste(
             "summary_table <- data.frame(",
             "  site = sort(unique(artifacts$site)),",
             "  n = as.vector(table(artifacts$site)),",
             "  mean_length = round(as.vector(tapply(artifacts$length_mm, artifacts$site, mean)), 2),",
             "  sd_length = round(as.vector(tapply(artifacts$length_mm, artifacts$site, sd)), 2)",
             ")", sep = "\n")))

# ===========================================================================
# Assignment 2: data visualization (10 mocks). The readability criterion is
# manual, so it has no expected points. Full automatic marks are 90.
# ===========================================================================

plot_chunk <- function(code) list(length_width_plot = code)
good_plot <- paste(
  "length_width_plot <- ggplot(artifacts, aes(x = length_mm, y = width_mm, colour = raw_material)) +",
  "  geom_point() +",
  "  labs(x = \"Length (mm)\", y = \"Width (mm)\", colour = \"Raw material\",",
  "       title = \"Artifact length against width\")", sep = "\n")

add_mock("a2", "a2_correct", "correct", "none (correct answer)",
         expect("a2"), plot_chunk(good_plot))

# Same plot built differently: mapping given in the layer, color spelled the
# American way, labels via xlab/ylab/ggtitle. Full marks.
add_mock("a2", "a2_alt_layer_mapping", "alternate_valid",
         "none (correct answer; mapping in the layer, color=, labels via xlab/ylab/ggtitle)",
         expect("a2"),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts) +",
           "  geom_point(aes(x = length_mm, y = width_mm, color = raw_material)) +",
           "  xlab(\"Length (mm)\") + ylab(\"Width (mm)\") +",
           "  ggtitle(\"Artifact length against width\")", sep = "\n")))

# geom_jitter draws points (with a little noise), so the point layer is satisfied.
add_mock("a2", "a2_alt_jitter", "alternate_valid",
         "none (correct answer using geom_jitter instead of geom_point)",
         expect("a2"),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts, aes(length_mm, width_mm, colour = raw_material)) +",
           "  geom_jitter(width = 0.2, height = 0.2) +",
           "  labs(x = \"Length (mm)\", y = \"Width (mm)\", title = \"Artifact length against width\")",
           sep = "\n")))

# No axis labels set, so both axes show the column names: axis_labels 0. Title is set.
add_mock("a2", "a2_unlabeled_axes", "mistake",
         "axes left with the default column names as labels",
         expect("a2", axis_labels = 0),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts, aes(length_mm, width_mm, colour = raw_material)) +",
           "  geom_point() +",
           "  ggtitle(\"Artifact length against width\")", sep = "\n")))

# Only the x label is written: 1 of 2 axes, so 15 x 0.5 = 7.5.
add_mock("a2", "a2_half_labeled", "mistake",
         "x axis labelled, y axis left with the default column name",
         expect("a2", axis_labels = 7.5),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts, aes(length_mm, width_mm, colour = raw_material)) +",
           "  geom_point() +",
           "  labs(x = \"Length (mm)\", title = \"Artifact length against width\")", sep = "\n")))

add_mock("a2", "a2_no_title", "mistake", "no plot title",
         expect("a2", title = 0),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts, aes(length_mm, width_mm, colour = raw_material)) +",
           "  geom_point() +",
           "  labs(x = \"Length (mm)\", y = \"Width (mm)\")", sep = "\n")))

# y shows thickness instead of width. x is right: 1 of 2 axes, so 20 x 0.5 = 10.
# The y label is written (not the default), so axis_labels is full.
add_mock("a2", "a2_wrong_mapping", "mistake",
         "y axis shows thickness_mm instead of width_mm",
         expect("a2", axes_mapped = 10),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts, aes(length_mm, thickness_mm, colour = raw_material)) +",
           "  geom_point() +",
           "  labs(x = \"Length (mm)\", y = \"Width (mm)\", title = \"Artifact length against width\")",
           sep = "\n")))

add_mock("a2", "a2_no_colour", "mistake", "points not coloured by raw material",
         expect("a2", colour_mapped = 0),
         plot_chunk(paste(
           "length_width_plot <- ggplot(artifacts, aes(length_mm, width_mm)) +",
           "  geom_point() +",
           "  labs(x = \"Length (mm)\", y = \"Width (mm)\", title = \"Artifact length against width\")",
           sep = "\n")))

# Plot saved as `scatter`, so length_width_plot does not exist. The code runs
# (renders 10) but all five plot criteria are missing: 0.
add_mock("a2", "a2_wrong_object_name", "mistake",
         "plot saved under the wrong name (scatter)",
         zero("a2", keep = "renders"),
         plot_chunk(sub("^length_width_plot", "scatter", good_plot)))

# ggplt() does not exist: error in scatter-plot, so renders 0 and nothing is created.
add_mock("a2", "a2_error_in_chunk", "mistake",
         "typo ggplt() in the scatter-plot chunk throws an error",
         zero("a2"),
         plot_chunk(sub("ggplot\\(", "ggplt(", good_plot)),
         error_chunk = "scatter-plot")

# ===========================================================================
# Assignment 3: statistical reasoning (10 mocks). `reasoning` is manual.
# Full automatic marks are 90.
# ===========================================================================

run_welch <- paste(
  "river <- two_sites$length_mm[two_sites$site == \"River Terrace\"]",
  "cave <- two_sites$length_mm[two_sites$site == \"Cave Mouth\"]",
  "test_result <- t.test(river, cave)",
  "t_statistic <- unname(test_result$statistic)",
  "p_value <- test_result$p.value",
  "conf_int <- as.numeric(test_result$conf.int)", sep = "\n")
pooled_d <- paste(
  "n1 <- length(river)",
  "n2 <- length(cave)",
  "pooled_sd <- sqrt(((n1 - 1) * var(river) + (n2 - 1) * var(cave)) / (n1 + n2 - 2))",
  "cohens_d <- (mean(river) - mean(cave)) / pooled_sd", sep = "\n")

# The run-test chunk has three placeholders; one block fills the first and the
# other two lines are deleted.
test_chunk <- function(code, t = "t_statistic", cohens = pooled_d) {
  list(t_statistic = code, p_value = NA_character_, conf_int = NA_character_, cohens_d = cohens)
}

text_good <- paste(
  "River Terrace artifacts are on average about 6.5 mm longer than Cave Mouth artifacts.",
  "The t-test is highly significant (p < 0.001) and the 95% confidence interval for the",
  "difference does not include zero. The effect size is large (Cohen's d is about 1.0),",
  "so the difference is substantial and not just statistically detectable.")

add_mock("a3", "a3_correct", "correct", "none (correct answer, Welch t-test)",
         expect("a3"), test_chunk(run_welch), text_good)

# lm() with site as the predictor is an equal-variance test: t = 4.235 (Welch
# 4.252, gap 0.016 <= 0.05), p = 7.12e-5 (gap 2e-6 <= 1e-4), CI 3.451 to 9.604
# (gaps 0.010 <= 0.1). Uses lm, which the method criterion accepts. Full marks.
add_mock("a3", "a3_alt_lm", "alternate_valid",
         "none (correct answer using lm() instead of t.test())",
         expect("a3"),
         test_chunk(paste(
           "fit <- lm(length_mm ~ site, data = two_sites)",
           "coefs <- summary(fit)$coefficients",
           "t_statistic <- coefs[\"siteRiver Terrace\", \"t value\"]",
           "p_value <- coefs[\"siteRiver Terrace\", \"Pr(>|t|)\"]",
           "conf_int <- confint(fit)[\"siteRiver Terrace\", ]", sep = "\n"),
           cohens = paste(
             "means <- tapply(two_sites$length_mm, two_sites$site, mean)",
             "sds <- tapply(two_sites$length_mm, two_sites$site, sd)",
             "ns <- table(two_sites$site)",
             "pooled_sd <- sqrt(sum((ns - 1) * sds^2) / (sum(ns) - 2))",
             "cohens_d <- unname((means[\"River Terrace\"] - means[\"Cave Mouth\"]) / pooled_sd)", sep = "\n")),
         paste("A linear model of length on site shows River Terrace artifacts are about 6.5 mm longer",
               "than Cave Mouth artifacts (p < 0.001). The effect size is large, Cohen's d about 1.0."))

# Groups taken in the other order: t = -4.25, CI = -9.59 to -3.46, d = -1.03.
# ignore_sign accepts all three. Full marks.
add_mock("a3", "a3_alt_order_reversed", "alternate_valid",
         "none (correct answer with the groups in the other order, so signs are reversed)",
         expect("a3"),
         test_chunk(sub("t.test\\(river, cave\\)", "t.test(cave, river)", run_welch),
                    cohens = sub("\\(mean\\(river\\) - mean\\(cave\\)\\)", "(mean(cave) - mean(river))", pooled_d)),
         paste("Cave Mouth artifacts are shorter than River Terrace artifacts by about 6.5 mm (p < 0.001).",
               "The effect size is large (Cohen's d is about -1.0 because Cave Mouth is the first group)."))

# var.equal = TRUE: same numbers as the lm() mock, all within tolerance. Full marks.
add_mock("a3", "a3_alt_var_equal", "alternate_valid",
         "none (correct answer using t.test with var.equal = TRUE)",
         expect("a3"),
         test_chunk(sub("t.test\\(river, cave\\)", "t.test(river, cave, var.equal = TRUE)", run_welch)),
         text_good)

# A paired t-test needs equal-length groups (38 vs 31), so t.test() throws
# "'x' and 'y' must have the same length" in run-test. renders 0; the three
# results and cohens_d are never created: 0. The code does call t.test, so the
# method check (which reads code) still gives 10. The student wrote nothing under
# Interpretation (placeholder only): 0.
add_mock("a3", "a3_error_paired_test", "mistake",
         "paired t-test on groups of different size throws an error in run-test; no interpretation written",
         zero("a3", keep = "method"),
         test_chunk(sub("t.test\\(river, cave\\)", "t.test(river, cave, paired = TRUE)", run_welch)),
         NULL, error_chunk = "run-test")

# Ridge Camp vs Hilltop: t = -3.008, p = 0.00383, CI -6.72 to -1.35, d = -0.669.
# None is within tolerance of River Terrace vs Cave Mouth (|t| 3.01 vs 4.25;
# p gap 0.0038 > 1e-4; CI gaps > 0.1; |d| 0.669 vs 1.025). The code uses t.test
# (10), runs (10), and the interpretation names the effect size and a direction (20).
add_mock("a3", "a3_wrong_sites", "mistake",
         "compared Ridge Camp with Hilltop instead of River Terrace with Cave Mouth",
         expect("a3", t_statistic = 0, p_value = 0, conf_int = 0, cohens_d = 0),
         test_chunk(gsub("two_sites", "artifacts",
                         gsub("River Terrace", "Ridge Camp", gsub("Cave Mouth", "Hilltop", run_welch)))),
         paste("Ridge Camp artifacts are shorter than Hilltop artifacts by about 4 mm. The effect size",
               "is moderate."))

# cohens_d left as NULL (cohens_d: 0). The text says River Terrace is longer and
# the sites differ, but never mentions effect size or Cohen: 1 of 2 ideas, so
# interpretation = 20 x 0.5 = 10.
add_mock("a3", "a3_omits_effect_size", "mistake",
         "effect size not computed and not mentioned in the interpretation",
         expect("a3", cohens_d = 0, interpretation = 10),
         list(t_statistic = run_welch, p_value = NA_character_, conf_int = NA_character_),
         paste("River Terrace artifacts are about 6.5 mm longer than Cave Mouth artifacts on average,",
               "and the difference is statistically significant (p < 0.001)."))

# Everything correct, but the Interpretation section still holds the template
# placeholder: no ideas covered, interpretation 0.
add_mock("a3", "a3_no_interpretation", "mistake",
         "interpretation left as the template placeholder",
         expect("a3", interpretation = 0),
         test_chunk(run_welch), NULL)

# p-value saved as `pval`, so p_value is missing: 0. Everything else correct.
add_mock("a3", "a3_missing_object", "mistake",
         "p-value saved under the wrong name (pval)",
         expect("a3", p_value = 0),
         test_chunk(sub("\np_value <-", "\npval <-", run_welch)),
         text_good)

# Wilcoxon rank-sum instead of a t-test. Hand-graded against the rubric:
#   t_statistic = W = 894.5 vs 4.25: 0.
#   p_value = 1.566e-4 vs 6.92e-5, gap 8.7e-5 <= 1e-4: full 10 (a known blind
#     spot: with an effect this strong the p-values are close).
#   conf_int = Hodges-Lehmann 3.7 to 10.6 vs 3.46 to 9.59, gaps 0.24 and 1.0: 0.
#   cohens_d correct (15), method uses wilcox.test not t.test/lm: 0,
#   interpretation covers both ideas (20), renders 10.   Total 55.
add_mock("a3", "a3_wrong_test_wilcox", "mistake",
         "used a Wilcoxon rank-sum test instead of a t-test",
         expect("a3", t_statistic = 0, conf_int = 0, method = 0),
         test_chunk(paste(
           "river <- two_sites$length_mm[two_sites$site == \"River Terrace\"]",
           "cave <- two_sites$length_mm[two_sites$site == \"Cave Mouth\"]",
           "test_result <- suppressWarnings(wilcox.test(river, cave, conf.int = TRUE))",
           "t_statistic <- unname(test_result$statistic)",
           "p_value <- test_result$p.value",
           "conf_int <- as.numeric(test_result$conf.int)", sep = "\n")),
         paste("River Terrace artifacts are longer than Cave Mouth artifacts (p < 0.001).",
               "The effect size is large, Cohen's d about 1.0."))

# ===========================================================================
# Write everything
# ===========================================================================

key <- list()
planted <- list()
for (m in mocks) {
  dir <- file.path(out_root, m$assignment)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  lines <- fill(template(m$assignment), m$code)
  if (!is.null(m$interpretation)) lines <- fill_interpretation(lines, m$interpretation)
  writeLines(lines, file.path(dir, paste0(m$name, ".qmd")))

  stopifnot(setequal(names(m$expected), names(full[[m$assignment]])))
  key[[length(key) + 1]] <- data.frame(
    assignment = m$assignment, submission = m$name,
    criterion = names(m$expected), expected_points = unname(m$expected),
    stringsAsFactors = FALSE)
  planted[[length(planted) + 1]] <- data.frame(
    assignment = m$assignment, submission = m$name, kind = m$kind,
    mistake = m$mistake, error_chunk = m$error_chunk, stringsAsFactors = FALSE)
}
key <- do.call(rbind, key)
planted <- do.call(rbind, planted)
utils::write.csv(key, file.path(out_root, "expected_scores.csv"), row.names = FALSE, eol = "\n")
utils::write.csv(planted, file.path(out_root, "planted_mistakes.csv"), row.names = FALSE, eol = "\n")

tot <- tapply(key$expected_points, key$submission, sum)
message("Wrote ", length(mocks), " mocks (", paste(table(planted$assignment), collapse = " / "), " per assignment).")
message("Mean expected total per assignment: ",
        paste(names(tapply(key$expected_points, key$assignment, sum)),
              round(tapply(key$expected_points, key$assignment, sum) / table(planted$assignment)), collapse = ", "))
