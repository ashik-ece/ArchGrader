library(ggplot2)

df <- data.frame(len = c(1, 2, 3), wid = c(2, 4, 5), mat = c("a", "b", "a"))
base_plot <- ggplot(df, aes(x = len, y = wid, colour = mat)) + geom_point()
full_plot <- base_plot + labs(x = "Length (mm)", y = "Width (mm)", title = "T")

ref_plot <- fake_run(list(p = full_plot), object_chunk = c(p = "plot-chunk"))
plot_params <- function(...) c(list(object = "p"), list(...))
run_with <- function(p, ...) fake_run(list(p = p), ...)

# --- plot: geometry --------------------------------------------------------

test_that("plot: the required geometry passes, another fails", {
  expect_equal(check_plot(plot_params(geom = "point"), run_with(base_plot), ref_plot)$credit, 1)
  line <- ggplot(df, aes(len, wid)) + geom_line()
  r <- check_plot(plot_params(geom = "point"), run_with(line), ref_plot)
  expect_equal(r$credit, 0)
  expect_match(r$note, "expected a point layer but found line")
})

test_that("plot: geom_jitter counts as points, and alternatives are accepted", {
  jit <- ggplot(df, aes(len, wid)) + geom_jitter()
  expect_equal(check_plot(plot_params(geom = "point"), run_with(jit), ref_plot)$credit, 1)
  col <- ggplot(df, aes(len)) + geom_histogram(bins = 3)
  expect_equal(check_plot(plot_params(geom = c("bar", "col")), run_with(col), ref_plot)$credit, 1)
})

test_that("plot: a plot with no layers fails the geometry check", {
  r <- check_plot(plot_params(geom = "point"), run_with(ggplot(df, aes(len, wid))), ref_plot)
  expect_equal(r$credit, 0)
  expect_match(r$note, "no layers")
})

# --- plot: mappings --------------------------------------------------------

test_that("plot: mappings are matched by variable, with partial credit", {
  want <- list(x = "len", y = "wid")
  expect_equal(check_plot(plot_params(mapping = want), run_with(base_plot), ref_plot)$credit, 1)
  wrong_y <- ggplot(df, aes(len, len)) + geom_point()
  r <- check_plot(plot_params(mapping = want), run_with(wrong_y), ref_plot)
  expect_equal(r$credit, 0.5)
  expect_match(r$note, "y should show `wid` but shows `len`")
  swapped <- ggplot(df, aes(wid, len)) + geom_point()
  expect_equal(check_plot(plot_params(mapping = want), run_with(swapped), ref_plot)$credit, 0)
})

test_that("plot: mappings set in the layer count, and color means colour", {
  layer_map <- ggplot(df) + geom_point(aes(x = len, y = wid, color = mat))
  r <- check_plot(plot_params(mapping = list(x = "len", y = "wid", color = "mat")), run_with(layer_map), ref_plot)
  expect_equal(r$credit, 1)
})

test_that("plot: an unmapped aesthetic is reported as not mapped", {
  r <- check_plot(plot_params(mapping = list(colour = "mat")), run_with(ggplot(df, aes(len, wid)) + geom_point()), ref_plot)
  expect_equal(r$credit, 0)
  expect_match(r$note, "colour should show `mat` but is not mapped")
})

test_that("plot: a variable inside an expression still counts", {
  p <- ggplot(df, aes(x = log(len), y = wid)) + geom_point()
  expect_equal(check_plot(plot_params(mapping = list(x = "len")), run_with(p), ref_plot)$credit, 1)
})

# --- plot: labels ----------------------------------------------------------

test_that("plot: default axis labels do not count as labelled", {
  r <- check_plot(plot_params(labels = c("x", "y")), run_with(base_plot), ref_plot)
  expect_equal(r$credit, 0)
  expect_match(r$note, "add a label for: x, y")
})

test_that("plot: written labels count, however they were set", {
  expect_equal(check_plot(plot_params(labels = c("x", "y", "title")), run_with(full_plot), ref_plot)$credit, 1)
  alt <- base_plot + xlab("Length (mm)") + ylab("Width (mm)") + ggtitle("T")
  expect_equal(check_plot(plot_params(labels = c("x", "y", "title")), run_with(alt), ref_plot)$credit, 1)
})

test_that("plot: one of two axes labelled earns half", {
  half <- base_plot + labs(x = "Length (mm)")
  expect_equal(check_plot(plot_params(labels = c("x", "y")), run_with(half), ref_plot)$credit, 0.5)
})

test_that("plot: restating the variable name is not a label", {
  same <- base_plot + labs(x = "len", y = "wid")
  expect_equal(check_plot(plot_params(labels = c("x", "y")), run_with(same), ref_plot)$credit, 0)
})

test_that("plot: a title is needed only when asked for", {
  expect_equal(check_plot(plot_params(labels = "title"), run_with(base_plot), ref_plot)$credit, 0)
  expect_equal(check_plot(plot_params(labels = "title"), run_with(base_plot + ggtitle("Hi")), ref_plot)$credit, 1)
  expect_equal(check_plot(plot_params(labels = "title"), run_with(base_plot + ggtitle("  ")), ref_plot)$credit, 0)
})

test_that("plot: the legend label counts as colour", {
  expect_equal(check_plot(plot_params(labels = "color"), run_with(base_plot), ref_plot)$credit, 0)
  expect_equal(check_plot(plot_params(labels = "colour"), run_with(base_plot + labs(colour = "Material")), ref_plot)$credit, 1)
})

# --- plot: combined and failure cases -------------------------------------

test_that("plot: several parts average", {
  r <- check_plot(plot_params(geom = "point", labels = c("x", "y")), run_with(base_plot), ref_plot)
  expect_equal(r$credit, 0.5)
})

test_that("plot: observed describes what the plot has", {
  r <- check_plot(plot_params(geom = "point"), run_with(full_plot), ref_plot)
  expect_match(r$observed, "layers: point; mappings: ")
  expect_match(r$observed, "x=len")
  expect_equal(r$object, "p")
  expect_equal(r$chunk, "chunk-a")   # the student's chunk, not the solution's
})

test_that("plot: missing, NULL, and non-plot objects score zero", {
  expect_match(check_plot(plot_params(geom = "point"), fake_run(list(q = 1)), ref_plot)$note, "was not created")
  expect_match(check_plot(plot_params(geom = "point"), run_with(NULL), ref_plot)$note, "still NULL")
  r <- check_plot(plot_params(geom = "point"), run_with(data.frame(a = 1)), ref_plot)
  expect_equal(r$credit, 0)
  expect_match(r$note, "is not a ggplot object")
})

test_that("plot: a rubric naming an object the solution lacks is a loud error", {
  expect_error(check_plot(plot_params(geom = "point"), run_with(base_plot), fake_run(list(other = 1))),
               "instructor solution does not create an object named 'p'")
})

# --- uses_function ---------------------------------------------------------

fake_run_code <- function(...) utils::modifyList(fake_run(), list(chunk_code = list(...)))

test_that("uses_function: any of the listed functions", {
  p <- list(functions = c("t.test", "lm"), mode = "any")
  expect_equal(check_uses_function(p, fake_run_code(a = "x <- t.test(1:5, 2:6)"), NULL)$credit, 1)
  expect_equal(check_uses_function(p, fake_run_code(a = "m <- lm(y ~ x, d)"), NULL)$credit, 1)
  r <- check_uses_function(p, fake_run_code(a = "w <- wilcox.test(1:5, 2:6)"), NULL)
  expect_equal(r$credit, 0)
  expect_match(r$note, "should use t.test or lm")
  expect_equal(r$expected, "uses t.test or lm")
})

test_that("uses_function: mode all gives partial credit", {
  p <- list(functions = c("t.test", "var"), mode = "all")
  r <- check_uses_function(p, fake_run_code(a = "t.test(x)"), NULL)
  expect_equal(r$credit, 0.5)
  expect_match(r$note, "should use var")
})

test_that("uses_function: namespaced calls count, comments and strings do not", {
  p <- list(functions = "t.test")
  expect_equal(check_uses_function(p, fake_run_code(a = "stats::t.test(x)"), NULL)$credit, 1)
  expect_equal(check_uses_function(p, fake_run_code(a = "# t.test(x)\ns <- 't.test(x)'"), NULL)$credit, 0)
})

test_that("uses_function: a function passed by name counts", {
  expect_equal(check_uses_function(list(functions = "median"), fake_run_code(a = "sapply(l, median)"), NULL)$credit, 1)
})

test_that("uses_function: a chunk that does not parse is skipped, not fatal", {
  p <- list(functions = "t.test")
  r <- check_uses_function(p, fake_run_code(broken = "x <- (1 +", ok = "t.test(a, b)"), NULL)
  expect_equal(r$credit, 1)
})

test_that("uses_function: no code means no credit", {
  expect_equal(check_uses_function(list(functions = "t.test"), fake_run(), NULL)$credit, 0)
})

# --- sections and prose ----------------------------------------------------

test_that("extract_prose drops the YAML header and code, keeps headings and text", {
  lines <- c("---", "title: x", "---", "", "# Top", "text one", "```{r a}", "x <- 1 # not prose", "```",
             "~~~", "tilde code", "~~~", "## Interpretation", "my answer", "")
  prose <- extract_prose(lines)
  expect_equal(prose, c("", "# Top", "text one", "## Interpretation", "my answer", ""))
})

test_that("extract_prose handles a file with no header", {
  expect_equal(extract_prose(c("# A", "b")), c("# A", "b"))
  expect_equal(extract_prose(character()), character())
})

test_that("section_text returns the lines under a heading up to the next same-level heading", {
  prose <- c("# Doc", "## Interpretation", "para one", "### Detail", "sub text", "## Next", "other")
  expect_equal(section_text(prose, "Interpretation"), c("para one", "### Detail", "sub text"))
  expect_equal(section_text(prose, "next"), "other")
  expect_null(section_text(prose, "Missing"))
  expect_null(section_text("no headings here", "x"))
})

test_that("section_text matches headings loosely and ignores trailing hashes", {
  expect_equal(section_text(c("## My interpretation ##", "words"), "Interpretation"), "words")
  expect_equal(section_text(c("##  INTERPRETATION", "words"), "interpretation"), "words")
})

test_that("section_text returns empty text for a heading with nothing under it", {
  expect_equal(section_text(c("## Interpretation", "## Next"), "Interpretation"), character())
})

# --- text_mentions ---------------------------------------------------------

tm_params <- list(heading = "Interpretation",
                  groups = list(effect_size = c("effect size", "cohen"), conclusion = c("longer", "differ")))
prose_run <- function(...) fake_run_prose(c("## Interpretation", ...))
fake_run_prose <- function(lines) utils::modifyList(fake_run(), list(prose = lines))

test_that("text_mentions: all ideas covered", {
  r <- check_text_mentions(tm_params, prose_run("Site A is LONGER. The Effect Size is large."), NULL)
  expect_equal(r$credit, 1)
  expect_equal(r$observed, "mentions: effect_size, conclusion")
})

test_that("text_mentions: partial credit names what is missing", {
  r <- check_text_mentions(tm_params, prose_run("Cohen's d is 1."), NULL)
  expect_equal(r$credit, 0.5)
  expect_match(r$note, "does not mention: conclusion")
  expect_equal(r$observed, "mentions: effect_size")
})

test_that("text_mentions: nothing covered, missing section, and empty section", {
  expect_equal(check_text_mentions(tm_params, prose_run("Nothing relevant."), NULL)$credit, 0)
  r <- check_text_mentions(tm_params, fake_run_prose(c("## Other", "longer")), NULL)
  expect_equal(r$credit, 0)
  expect_match(r$note, "No section headed `Interpretation`")
  r <- check_text_mentions(tm_params, fake_run_prose("## Interpretation"), NULL)
  expect_equal(r$credit, 0)
  expect_match(r$note, "is empty")
  expect_equal(check_text_mentions(tm_params, fake_run(), NULL)$credit, 0)
})

test_that("text_mentions: only the section under the heading counts", {
  lines <- c("## Task", "The effect size and longer appear here.", "## Interpretation", "Nothing.")
  expect_equal(check_text_mentions(tm_params, fake_run_prose(lines), NULL)$credit, 0)
})

# --- value: ignore_sign ----------------------------------------------------

test_that("value ignore_sign: a negated number is accepted, only when asked", {
  ref <- fake_run(list(t = 4.25))
  p <- list(object = "t", tolerance = 0.05, ignore_sign = TRUE)
  expect_equal(check_value(p, fake_run(list(t = -4.25)), ref)$credit, 1)
  p$ignore_sign <- NULL
  expect_equal(check_value(p, fake_run(list(t = -4.25)), ref)$credit, 0)
})

test_that("value ignore_sign: an interval for the groups in the other order is accepted", {
  ref <- fake_run(list(ci = c(3.5, 9.6)))
  p <- list(object = "ci", tolerance = 0.1, ignore_sign = TRUE)
  expect_equal(check_value(p, fake_run(list(ci = c(-9.6, -3.5))), ref)$credit, 1)
  expect_equal(check_value(p, fake_run(list(ci = c(3.5, 9.6))), ref)$credit, 1)
  # a plain sign flip without reordering is not an interval for the other order
  expect_lt(check_value(p, fake_run(list(ci = c(-3.5, -9.6))), ref)$credit, 1)
  expect_equal(check_value(p, fake_run(list(ci = c(1, 2))), ref)$credit, 0)
})

test_that("value ignore_sign: a named numeric (as from confint) is fine", {
  ref <- fake_run(list(ci = c(3.5, 9.6)))
  got <- c("2.5 %" = 3.46, "97.5 %" = 9.59)
  expect_equal(check_value(list(object = "ci", tolerance = 0.1), fake_run(list(ci = got)), ref)$credit, 1)
})
