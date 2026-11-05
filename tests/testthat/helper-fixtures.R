# Helpers shared by the tests.

# Write a .qmd made of R chunks. `chunks` is a character vector of code; names
# become header labels (```{r name}). Returns the file path.
write_qmd <- function(chunks, dir = withr::local_tempdir(.local_envir = parent.frame()),
                      name = "sub.qmd", header = c("---", "title: test", "---", "")) {
  body <- unlist(Map(function(code, label) {
    open <- if (nzchar(label)) paste0("```{r ", label, "}") else "```{r}"
    c(open, code, "```", "")
  }, chunks, names(chunks) %||% rep("", length(chunks))), use.names = FALSE)
  path <- file.path(dir, name)
  writeLines(c(header, body), path)
  path
}

# A valid one-criterion rubric as an R list; override pieces to break it.
rubric_list <- function(...) {
  base <- list(
    assignment = "t", title = "Test", solution = "sol.qmd", total_points = 10,
    criteria = list(list(
      id = "x", description = "A value", points = 10, check = "value",
      params = list(object = "x", tolerance = 0.01),
      comments = list(pass = "ok {object}", partial = "some", fail = "no {expected}"),
      hint = "try {chunk}"
    ))
  )
  utils::modifyList(base, list(...), keep.null = TRUE)
}

write_rubric <- function(x, dir = withr::local_tempdir(.local_envir = parent.frame()),
                         name = "rubric.yaml") {
  path <- file.path(dir, name)
  yaml::write_yaml(x, path)
  path
}

# A run result shaped like run_submission()'s, for testing checks in isolation.
fake_run <- function(objects = list(), object_chunk = NULL, error = NULL,
                     error_chunk = NULL, timed_out = FALSE) {
  if (is.null(object_chunk)) object_chunk <- stats::setNames(rep("chunk-a", length(objects)), names(objects))
  list(objects = objects, object_chunk = object_chunk, error = error,
       error_chunk = error_chunk, timed_out = timed_out, elapsed = 0, chunks = "chunk-a", code = "")
}
