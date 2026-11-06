# Checks that look at how an answer was produced or written rather than at a
# number: plot, uses_function and text_mentions. Same contract as checks.R:
# they take the criterion's params and the student and reference run results,
# and return check_result(). A bad submission is a low credit, never an error.

# Shared start of a check on a named object: the rubric must name an object the
# solution creates (otherwise it is the instructor's mistake and is loud), and
# the student's object may be missing or NULL. Returns either a finished
# check_result (when the student's object is unusable) or NULL.
object_problem <- function(obj, student, reference, expected_text) {
  if (!obj %in% names(reference$objects) || is.null(reference$objects[[obj]])) {
    stop("The instructor solution does not create an object named '", obj,
         "', so the rubric cannot check it.", call. = FALSE)
  }
  ref_chunk <- reference$object_chunk[obj]
  ref_chunk <- if (length(ref_chunk) && !is.na(ref_chunk)) unname(ref_chunk) else NA_character_
  fail <- function(why, observed) {
    check_result(0, paste0("Object `", obj, "` ", why), observed = observed,
                 expected = expected_text, object = obj, chunk = ref_chunk)
  }
  if (!obj %in% names(student$objects)) return(fail("was not created.", "(not created)"))
  if (is.null(student$objects[[obj]])) return(fail("was never given a value (it is still NULL).", "(NULL)"))
  NULL
}

student_chunk <- function(obj, student, reference) {
  chunk <- unname(student$object_chunk[obj])
  if (length(chunk) && !is.na(chunk)) return(chunk)
  ref <- unname(reference$object_chunk[obj])
  if (length(ref) && !is.na(ref)) ref else NA_character_
}

# --- plot ------------------------------------------------------------------

# A ggplot object has the right geometry, mappings and labels. Credit is the
# average over the parts the criterion asks about, and a part with several
# items (several aesthetics, several labels) earns the share that are right.
# The plot object is inspected, not drawn.
check_plot <- function(params, student, reference) {
  obj <- params$object
  parts <- intersect(c("geom", "mapping", "labels"), names(params))
  expected_text <- describe_plot_request(params)

  early <- object_problem(obj, student, reference, expected_text)
  if (!is.null(early)) return(early)

  p <- student$objects[[obj]]
  chunk <- student_chunk(obj, student, reference)
  if (!inherits(p, "ggplot")) {
    return(check_result(0, paste0("Object `", obj, "` is not a ggplot object (it is ", class(p)[1], ")."),
                        observed = class(p)[1], expected = expected_text, object = obj, chunk = chunk))
  }

  results <- list()
  if ("geom" %in% parts) results$geom <- plot_geom_part(p, unlist(params$geom))
  if ("mapping" %in% parts) results$mapping <- plot_mapping_part(p, params$mapping)
  if ("labels" %in% parts) results$labels <- plot_labels_part(p, unlist(params$labels))

  credit <- mean(vapply(results, function(r) r$credit, numeric(1)))
  details <- unlist(lapply(results, function(r) r$problems), use.names = FALSE)
  note <- if (credit >= 1 - 1e-9) "The plot has what the task asks for." else paste0(paste(details, collapse = "; "), ".")
  check_result(credit, note, observed = describe_plot(p), expected = expected_text,
               object = obj, chunk = chunk)
}

plot_geoms <- function(p) {
  tolower(sub("^Geom", "", vapply(p$layers, function(l) class(l$geom)[1], "")))
}

plot_geom_part <- function(p, wanted) {
  have <- plot_geoms(p)
  ok <- any(tolower(wanted) %in% have)
  list(credit = as.numeric(ok),
       problems = if (ok) character() else
         paste0("expected a ", paste(wanted, collapse = " or "), " layer but found ",
                if (length(have)) paste(have, collapse = ", ") else "no layers"))
}

# The expression mapped to an aesthetic: layer mappings win over the plot's.
plot_aes_expr <- function(p, aes_name) {
  maps <- c(lapply(p$layers, function(l) l$mapping), list(p$mapping))
  for (m in maps) {
    if (!is.null(m) && aes_name %in% names(m)) return(m[[aes_name]])
  }
  NULL
}

as_expression <- function(q) if (rlang::is_quosure(q)) rlang::quo_get_expr(q) else q

normalise_aes <- function(x) sub("^color$", "colour", x)

plot_mapping_part <- function(p, wanted) {
  names(wanted) <- normalise_aes(names(wanted))
  ok <- vapply(names(wanted), function(a) {
    q <- plot_aes_expr(p, a)
    !is.null(q) && wanted[[a]] %in% all.vars(as_expression(q))
  }, logical(1))
  problems <- vapply(names(wanted)[!ok], function(a) {
    q <- plot_aes_expr(p, a)
    paste0(a, " should show `", wanted[[a]], "`",
           if (is.null(q)) " but is not mapped" else paste0(" but shows `", rlang::as_label(q), "`"))
  }, "")
  list(credit = mean(ok), problems = unname(problems))
}

# Final labels of a plot, defaults included, across ggplot2 versions.
plot_final_labels <- function(p) {
  get_labs <- get0("get_labs", envir = asNamespace("ggplot2"), inherits = FALSE)
  labs <- tryCatch(
    if (is.function(get_labs)) get_labs(p) else ggplot2::ggplot_build(p)$plot$labels,
    error = function(e) p$labels
  )
  labs
}

label_text <- function(x) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste(x, collapse = " "))
  if (is.language(x) || inherits(x, "formula")) return(paste(deparse(x), collapse = " "))
  ""
}

# An axis or legend label counts as written when it is not just the name of
# the mapped variable, which is what ggplot2 shows when nothing was set.
plot_labels_part <- function(p, wanted) {
  wanted <- unique(normalise_aes(wanted))
  labs <- plot_final_labels(p)
  ok <- vapply(wanted, function(k) {
    text <- trimws(label_text(labs[[k]]))
    if (!nzchar(text)) return(FALSE)
    if (k %in% c("title", "subtitle", "caption")) return(TRUE)
    q <- plot_aes_expr(p, k)
    default <- if (is.null(q)) NA_character_ else rlang::as_label(q)
    !identical(text, default)
  }, logical(1))
  list(credit = mean(ok),
       problems = if (all(ok)) character() else
         paste0("add a label for: ", paste(wanted[!ok], collapse = ", ")))
}

describe_plot <- function(p) {
  geoms <- plot_geoms(p)
  aes_names <- unique(unlist(c(lapply(p$layers, function(l) names(l$mapping)), list(names(p$mapping)))))
  maps <- vapply(aes_names, function(a) paste0(a, "=", rlang::as_label(plot_aes_expr(p, a))), "")
  paste0("layers: ", if (length(geoms)) paste(geoms, collapse = ", ") else "none",
         "; mappings: ", if (length(maps)) paste(maps, collapse = ", ") else "none")
}

describe_plot_request <- function(params) {
  bits <- character()
  if (!is.null(params$geom)) bits <- c(bits, paste0("layer: ", paste(unlist(params$geom), collapse = " or ")))
  if (!is.null(params$mapping)) {
    bits <- c(bits, paste0("mappings: ", paste(names(params$mapping), unlist(params$mapping), sep = "=", collapse = ", ")))
  }
  if (!is.null(params$labels)) bits <- c(bits, paste0("labels: ", paste(unlist(params$labels), collapse = ", ")))
  paste(bits, collapse = "; ")
}

# --- uses_function ---------------------------------------------------------

# A required function appears in the code. Looks at names in the parsed code,
# so comments and strings do not count, and `stats::t.test()` counts as
# `t.test`. This is the one check that reads code instead of results, so use
# it sparingly: only when the task requires a specific method.
check_uses_function <- function(params, student, reference) {
  wanted <- unlist(params$functions)
  mode <- params$mode %||% "any"
  used <- code_names(student$chunk_code %||% list())
  found <- wanted %in% used

  credit <- if (identical(mode, "all")) mean(found) else as.numeric(any(found))
  joiner <- if (identical(mode, "all")) " and " else " or "
  expected <- paste0("uses ", paste(wanted, collapse = joiner))
  observed <- if (any(found)) paste0("uses ", paste(wanted[found], collapse = ", ")) else "none of the required functions"
  note <- if (credit >= 1 - 1e-9) {
    paste0("The code uses ", paste(wanted[found], collapse = ", "), ".")
  } else {
    paste0("The code should use ", paste(wanted[!found], collapse = joiner), ".")
  }
  check_result(credit, note, observed = observed, expected = expected)
}

# Every function and variable name in the parsed code. A chunk that does not
# parse is skipped (the runner already reports that as an error).
code_names <- function(chunk_code) {
  unique(unlist(lapply(chunk_code, function(code) {
    exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) character() else all.names(exprs)
  }), use.names = FALSE))
}

# --- text_mentions ---------------------------------------------------------

# The written interpretation covers the required ideas. Finds the section under
# the heading and checks, for each idea (a named list of alternative keywords),
# whether any keyword appears. Credit is the share of ideas covered.
#
# This is a keyword check. It can confirm that "effect size" is mentioned; it
# cannot confirm the reasoning is right, so pair it with a `manual` criterion.
check_text_mentions <- function(params, student, reference) {
  groups <- lapply(params$groups, unlist)
  expected <- paste0("mentions: ", paste(names(groups), collapse = ", "))

  text <- section_text(student$prose %||% character(), params$heading)
  if (is.null(text)) {
    return(check_result(0, paste0("No section headed `", params$heading, "` was found."),
                        observed = "(section not found)", expected = expected))
  }
  body <- tolower(paste(text, collapse = " "))
  hit <- vapply(groups, function(kws) any(vapply(tolower(kws), grepl, logical(1), x = body, fixed = TRUE)), logical(1))

  if (!nzchar(trimws(body))) {
    return(check_result(0, paste0("The section `", params$heading, "` is empty."),
                        observed = "(empty)", expected = expected))
  }
  credit <- mean(hit)
  observed <- paste0("mentions: ", if (any(hit)) paste(names(groups)[hit], collapse = ", ") else "none")
  note <- if (all(hit)) {
    "The interpretation covers every required idea (keyword check; a person should still read it)."
  } else {
    paste0("The interpretation does not mention: ", paste(names(groups)[!hit], collapse = ", "), ".")
  }
  check_result(credit, note, observed = observed, expected = expected)
}

# The lines under the first heading containing `heading` (case-insensitive),
# up to the next heading of the same or higher level; NULL if there is none.
section_text <- function(prose, heading) {
  is_heading <- grepl("^\\s{0,3}#{1,6}\\s+\\S", prose)
  if (!any(is_heading)) return(NULL)
  level <- ifelse(is_heading, nchar(sub("^\\s{0,3}(#+).*$", "\\1", prose)), NA_integer_)
  title <- ifelse(is_heading, tolower(trimws(sub("\\s*#*\\s*$", "", sub("^\\s{0,3}#+\\s+", "", prose)))), NA_character_)
  start <- which(is_heading & grepl(tolower(heading), title, fixed = TRUE))
  if (length(start) == 0) return(NULL)
  start <- start[1]
  later <- which(is_heading & seq_along(prose) > start & level <= level[start])
  end <- if (length(later)) later[1] - 1L else length(prose)
  if (end <= start) return(character())
  prose[(start + 1L):end]
}
