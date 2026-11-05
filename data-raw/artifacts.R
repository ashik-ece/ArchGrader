# Generates the synthetic archaeology dataset used by every assignment.
#
# Run from the project root:   Rscript data-raw/artifacts.R
#
# The data are synthetic: no licensing questions and a known right answer.
# The seed is fixed and the output is rounded and written with fixed options,
# so running this script twice gives byte-identical CSV files.

set.seed(20261106)

n <- 150

sites <- c("Ridge Camp", "River Terrace", "Cave Mouth", "Hilltop")
site_weights <- c(0.35, 0.30, 0.20, 0.15)

materials <- c("chert", "obsidian", "quartzite", "basalt")
material_weights <- c(0.40, 0.20, 0.25, 0.15)

# Mean artifact length (mm) differs by site, so Assignment 3 (a two-site
# t-test) has a real effect to find. Raw material shifts length slightly.
site_effect <- c("Ridge Camp" = 0, "River Terrace" = 4.5,
                 "Cave Mouth" = -2.0, "Hilltop" = 1.5)
material_effect <- c(chert = 0, obsidian = -3, quartzite = 2.5, basalt = 4)

site <- sample(sites, n, replace = TRUE, prob = site_weights)
raw_material <- sample(materials, n, replace = TRUE, prob = material_weights)

length_mm <- 38 + site_effect[site] + material_effect[raw_material] +
  rnorm(n, mean = 0, sd = 6)
# Width is correlated with length; thickness with width.
width_mm <- 0.55 * length_mm + rnorm(n, mean = 4, sd = 3)
thickness_mm <- 0.25 * width_mm + rnorm(n, mean = 2, sd = 1.2)

# Keep every measurement plausible and strictly positive.
length_mm <- pmax(length_mm, 12)
width_mm <- pmax(width_mm, 8)
thickness_mm <- pmax(thickness_mm, 2)

artifacts <- data.frame(
  artifact_id = sprintf("A%03d", seq_len(n)),
  site = unname(site),
  raw_material = unname(raw_material),
  length_mm = round(unname(length_mm), 1),
  width_mm = round(unname(width_mm), 1),
  thickness_mm = round(unname(thickness_mm), 1),
  stringsAsFactors = FALSE
)

out <- file.path("inst", "assignments", "artifacts.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write.csv(artifacts, out, row.names = FALSE, eol = "\n", fileEncoding = "UTF-8")
message("Wrote ", nrow(artifacts), " rows to ", out)
