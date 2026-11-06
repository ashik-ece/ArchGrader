#' Check types a rubric may use
#'
#' The complete, fixed set of check types. Adding a criterion means editing
#' the rubric, never R code, so this set stays small.
#' @noRd
CHECK_TYPES <- c("renders", "value", "plot", "uses_function", "text_mentions", "manual")

# Parameters accepted by each check type.
CHECK_PARAMS <- list(
  renders       = list(required = character(), optional = character()),
  value         = list(required = c("object", "tolerance"), optional = c("key", "ignore_sign")),
  plot          = list(required = "object", optional = c("geom", "mapping", "labels")),
  uses_function = list(required = "functions", optional = "mode"),
  text_mentions = list(required = c("heading", "groups"), optional = character()),
  manual        = list(required = character(), optional = character())
)

# Plot labels a rubric may ask for ("color" is accepted and means "colour").
PLOT_LABEL_KEYS <- c("x", "y", "title", "subtitle", "caption", "colour", "color", "fill")

RUBRIC_TOP_REQUIRED <- c("assignment", "title", "solution", "total_points", "criteria")
RUBRIC_TOP_OPTIONAL <- "timeout"
CRITERION_REQUIRED  <- c("id", "description", "points", "check")
CRITERION_OPTIONAL  <- c("params", "comments", "hint")
COMMENT_KEYS        <- c("pass", "partial", "fail")

# Column names scores.csv uses for itself; a criterion id must not clash.
RESERVED_IDS <- c("student_id", "total", "error", "error_chunk", "seconds")

# Placeholders the feedback report fills in from the check results.
PLACEHOLDERS <- c("object", "observed", "expected", "chunk")

#' Load and validate a grading rubric
#'
#' Reads a YAML rubric and checks it before anything is graded, so a typo
#' fails loudly on load instead of silently changing a grade. All problems
#' found are reported together.
#'
#' A rubric has the top-level fields `assignment`, `title`, `solution` (file
#' name of the instructor solution), `total_points`, `criteria`, and an
#' optional `timeout` (seconds). Each criterion has `id`, `description`,
#' `points`, `check`, `params`, `comments` (`pass`, `partial`, `fail`) and
#' `hint`. Comments and hints may contain the placeholders `{object}`,
#' `{observed}`, `{expected}` and `{chunk}`. See `docs/rubric-format.md`.
#'
#' @param path Path to a rubric `.yaml` file.
#' @return A list of class `archgrader_rubric` with the validated fields.
#'   `criteria` is a list with one element per criterion.
#' @export
load_rubric <- function(path) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("Rubric file not found: ", format(path), call. = FALSE)
  }
  raw <- tryCatch(
    yaml::read_yaml(path, handlers = list("bool#yes" = yaml_bool, "bool#no" = yaml_bool)),
    error = function(e) {
      stop("Rubric '", basename(path), "' is not valid YAML: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  problems <- validate_rubric(raw)
  if (length(problems) > 0) {
    stop(
      "Invalid rubric '", basename(path), "' (", length(problems),
      if (length(problems) == 1) " problem" else " problems", "):\n",
      paste0("  - ", problems, collapse = "\n"),
      call. = FALSE
    )
  }
  raw$criteria <- lapply(raw$criteria, function(cr) {
    if (is.null(cr$params)) cr$params <- list()
    cr
  })
  raw$path <- normalizePath(path)
  structure(raw, class = "archgrader_rubric")
}

# YAML 1.1 reads a bare y, n, yes, no, on or off as a boolean. That would turn
# the plot aesthetic `y` into the key TRUE, or the keyword "no" into FALSE,
# without any error. Only true and false are booleans in a rubric; everything
# else stays the text it obviously is.
yaml_bool <- function(x) {
  if (tolower(x) %in% c("true", "false")) as.logical(toupper(x)) else x
}

# Returns a character vector of problems; empty means the rubric is valid.
validate_rubric <- function(x) {
  problems <- character()
  add <- function(...) problems <<- c(problems, paste0(...))

  if (!is.list(x) || is.null(names(x))) {
    return("the file must be a YAML mapping with the fields assignment, title, solution, total_points and criteria")
  }

  # --- top level -----------------------------------------------------------
  unknown <- setdiff(names(x), c(RUBRIC_TOP_REQUIRED, RUBRIC_TOP_OPTIONAL))
  if (length(unknown)) add("unknown top-level field(s): ", paste(unknown, collapse = ", "))
  missing <- setdiff(RUBRIC_TOP_REQUIRED, names(x))
  if (length(missing)) add("missing top-level field(s): ", paste(missing, collapse = ", "))

  for (f in c("assignment", "title", "solution")) {
    if (f %in% names(x) && !is_string(x[[f]])) add("'", f, "' must be a non-empty text value")
  }
  if ("timeout" %in% names(x) && !(is_number(x$timeout) && x$timeout > 0)) {
    add("'timeout' must be a positive number of seconds")
  }
  if ("total_points" %in% names(x) && !(is_number(x$total_points) && x$total_points > 0)) {
    add("'total_points' must be a positive number")
  }

  # --- criteria ------------------------------------------------------------
  crit <- x$criteria
  if ("criteria" %in% names(x) && (!is.list(crit) || length(crit) == 0)) {
    add("'criteria' must be a non-empty list")
    return(problems)
  }
  if (is.null(crit)) return(problems)

  ids <- vapply(crit, function(cr) if (is.list(cr) && is_string(cr$id)) cr$id else NA_character_, "")
  dup <- unique(ids[!is.na(ids) & duplicated(ids)])
  if (length(dup)) add("duplicate criterion id(s): ", paste(dup, collapse = ", "))

  for (i in seq_along(crit)) {
    cr <- crit[[i]]
    label <- if (!is.na(ids[i])) paste0("criterion '", ids[i], "'") else paste0("criterion #", i)
    cadd <- function(...) add(label, ": ", ...)

    if (!is.list(cr) || is.null(names(cr))) {
      cadd("must be a mapping of fields")
      next
    }
    unknown <- setdiff(names(cr), c(CRITERION_REQUIRED, CRITERION_OPTIONAL))
    if (length(unknown)) cadd("unknown field(s): ", paste(unknown, collapse = ", "))
    missing <- setdiff(CRITERION_REQUIRED, names(cr))
    if (length(missing)) cadd("missing field(s): ", paste(missing, collapse = ", "))

    if ("id" %in% names(cr) && !(is_string(cr$id) && grepl("^[A-Za-z][A-Za-z0-9_]*$", cr$id))) {
      cadd("'id' must start with a letter and use only letters, digits and underscores")
    } else if (is_string(cr$id) && cr$id %in% RESERVED_IDS) {
      cadd("'id' cannot be '", cr$id, "' because scores.csv already has a column with that name. ",
           "Reserved: ", paste(RESERVED_IDS, collapse = ", "))
    }
    if ("description" %in% names(cr) && !is_string(cr$description)) {
      cadd("'description' must be a non-empty text value")
    }
    if ("points" %in% names(cr) && !(is_number(cr$points) && cr$points > 0)) {
      cadd("'points' must be a positive number")
    }

    check <- cr$check
    check_ok <- FALSE
    if ("check" %in% names(cr)) {
      if (!is_string(check) || !check %in% CHECK_TYPES) {
        cadd("unknown check type '", format(check), "'. Valid types: ",
             paste(CHECK_TYPES, collapse = ", "))
      } else {
        check_ok <- TRUE
      }
    }

    # params, for check types that are implemented
    if (check_ok) {
      spec <- CHECK_PARAMS[[check]]
      params <- cr$params
      if (!is.null(params) && (!is.list(params) || (length(params) > 0 && is.null(names(params))))) {
        cadd("'params' must be a mapping")
      } else {
        have <- names(params)
        bad <- setdiff(have, c(spec$required, spec$optional))
        if (length(bad)) {
          cadd("unknown parameter(s) for check '", check, "': ", paste(bad, collapse = ", "),
               if (length(c(spec$required, spec$optional))) {
                 paste0(". Allowed: ", paste(c(spec$required, spec$optional), collapse = ", "))
               } else ". This check takes no parameters")
        }
        miss <- setdiff(spec$required, have)
        if (length(miss)) cadd("check '", check, "' needs parameter(s): ", paste(miss, collapse = ", "))
        if ("object" %in% have && !is_string(params$object)) cadd("param 'object' must be a text value")
        if ("tolerance" %in% have && !(is_number(params$tolerance) && params$tolerance >= 0)) {
          cadd("param 'tolerance' must be a number of 0 or more")
        }
        if ("key" %in% have && !(is.character(unlist(params$key)) && length(params$key) >= 1)) {
          cadd("param 'key' must be a column name or a list of column names")
        }
        if ("ignore_sign" %in% have && !is.logical(params$ignore_sign)) {
          cadd("param 'ignore_sign' must be true or false")
        }
        for (msg in type_param_problems(check, params)) cadd(msg)
      }
    }

    # comments and hint
    is_manual <- check_ok && check == "manual"
    comments <- cr$comments
    if (is.null(comments)) {
      if (!is_manual) cadd("missing field 'comments' (needs pass, partial and fail)")
    } else if (!is.list(comments) || is.null(names(comments))) {
      cadd("'comments' must be a mapping with pass, partial and fail")
    } else {
      bad <- setdiff(names(comments), COMMENT_KEYS)
      if (length(bad)) cadd("unknown comment key(s): ", paste(bad, collapse = ", "), ". Use pass, partial, fail")
      miss <- setdiff(COMMENT_KEYS, names(comments))
      if (length(miss) && !is_manual) cadd("'comments' is missing: ", paste(miss, collapse = ", "))
      for (k in intersect(names(comments), COMMENT_KEYS)) {
        if (!is_string(comments[[k]])) cadd("comment '", k, "' must be non-empty text")
        else add_placeholder_problems(comments[[k]], paste0(label, ": comment '", k, "'"), add)
      }
    }
    if (is.null(cr$hint)) {
      if (!is_manual) cadd("missing field 'hint'")
    } else if (!is_string(cr$hint)) {
      cadd("'hint' must be non-empty text")
    } else {
      add_placeholder_problems(cr$hint, paste0(label, ": hint"), add)
    }
  }

  # --- total ---------------------------------------------------------------
  pts <- vapply(crit, function(cr) {
    if (is.list(cr) && is_number(cr$points)) as.numeric(cr$points) else NA_real_
  }, numeric(1))
  if (is_number(x$total_points) && !anyNA(pts) && abs(sum(pts) - x$total_points) > 1e-8) {
    add("criterion points add up to ", format(sum(pts)), " but total_points is ", format(x$total_points))
  }

  problems
}

# Extra checks on the parameters of the newer check types.
type_param_problems <- function(check, params) {
  out <- character()
  if (check == "plot") {
    wanted <- intersect(c("geom", "mapping", "labels"), names(params))
    if (length(wanted) == 0) {
      out <- c(out, "check 'plot' needs at least one of: geom, mapping, labels")
    }
    if ("geom" %in% names(params) && !all_strings(params$geom)) {
      out <- c(out, "param 'geom' must be a geometry name such as point, or a list of names")
    }
    if ("mapping" %in% names(params)) {
      m <- params$mapping
      if (!is.list(m) || length(m) == 0 || is.null(names(m)) || !all(vapply(m, is_string, logical(1)))) {
        out <- c(out, "param 'mapping' must map aesthetic names to variable names, for example x: length_mm")
      }
    }
    if ("labels" %in% names(params)) {
      keys <- unlist(params$labels)
      if (!all_strings(keys) || !all(keys %in% PLOT_LABEL_KEYS)) {
        out <- c(out, paste0("param 'labels' must be a list drawn from: ", paste(PLOT_LABEL_KEYS, collapse = ", ")))
      }
    }
  }
  if (check == "uses_function") {
    if ("functions" %in% names(params) && !all_strings(params$functions)) {
      out <- c(out, "param 'functions' must be a list of function names")
    }
    if ("mode" %in% names(params) && !(is_string(params$mode) && params$mode %in% c("any", "all"))) {
      out <- c(out, "param 'mode' must be 'any' or 'all'")
    }
  }
  if (check == "text_mentions") {
    if ("heading" %in% names(params) && !is_string(params$heading)) {
      out <- c(out, "param 'heading' must be non-empty text")
    }
    g <- params$groups
    if ("groups" %in% names(params) &&
        (!is.list(g) || length(g) == 0 || is.null(names(g)) || any(!nzchar(names(g))) ||
         !all(vapply(g, all_strings, logical(1))))) {
      out <- c(out, "param 'groups' must map a group name to a list of keywords, for example effect_size: [effect size, cohen]")
    }
  }
  out
}

all_strings <- function(x) {
  x <- unlist(x)
  is.character(x) && length(x) >= 1 && !anyNA(x) && all(nzchar(trimws(x)))
}

add_placeholder_problems <- function(text, where, add) {
  found <- regmatches(text, gregexpr("\\{[^{}]*\\}", text))[[1]]
  found <- gsub("[{}]", "", found)
  bad <- setdiff(found, PLACEHOLDERS)
  if (length(bad)) {
    add(where, " uses unknown placeholder(s): ", paste0("{", bad, "}", collapse = ", "),
        ". Allowed: ", paste0("{", PLACEHOLDERS, "}", collapse = ", "))
  }
}

is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(x))
is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)

#' @export
print.archgrader_rubric <- function(x, ...) {
  cat("<archgrader_rubric> ", x$assignment, ": ", x$title, "\n", sep = "")
  cat(length(x$criteria), " criteria, ", format(x$total_points), " points\n", sep = "")
  invisible(x)
}
