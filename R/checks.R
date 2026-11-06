# Check functions
#
# One small function per check type. Each takes the criterion's `params`, the
# student's run result and the reference (solution) run result, and returns
# the list built by check_result(): a credit between 0 and 1 and a short
# observed-versus-expected note. They never throw for a bad submission; a bad
# submission is just a low credit. They do throw for a bad rubric (for example
# a value check naming an object the instructor solution does not create),
# because that is the instructor's mistake and must be loud.

check_result <- function(credit, note, observed = NA_character_, expected = NA_character_,
                         object = NA_character_, chunk = NA_character_) {
  # NA credit is allowed: it marks a criterion that is not scored (manual).
  stopifnot(is.numeric(credit), is.na(credit) || (credit >= 0 && credit <= 1))
  list(credit = credit, note = note, observed = observed, expected = expected,
       object = object, chunk = chunk)
}

#' Dispatch one rubric criterion to its check function
#' @noRd
run_check <- function(criterion, student, reference) {
  params <- criterion$params
  switch(
    criterion$check,
    renders = check_renders(params, student, reference),
    value   = check_value(params, student, reference),
    plot    = check_plot(params, student, reference),
    uses_function = check_uses_function(params, student, reference),
    text_mentions = check_text_mentions(params, student, reference),
    manual  = check_manual(params, student, reference),
    stop("Check type '", criterion$check, "' (criterion '", criterion$id,
         "') is not implemented yet.", call. = FALSE)
  )
}

# --- renders ---------------------------------------------------------------

# The code runs top to bottom without error. 1 or 0.
check_renders <- function(params, student, reference) {
  if (is.null(student$error)) {
    return(check_result(1, "The code ran from top to bottom without errors.",
                        observed = "no errors", expected = "no errors"))
  }
  where <- if (!is.null(student$error_chunk)) paste0(" in chunk `", student$error_chunk, "`") else ""
  note <- if (isTRUE(student$timed_out)) {
    paste0("The code was stopped", where, ". ", student$error)
  } else {
    paste0("The code stopped with an error", where, ": ", student$error)
  }
  check_result(0, note, observed = student$error, expected = "no errors",
               chunk = student$error_chunk %||% NA_character_)
}

# --- value -----------------------------------------------------------------

# A number, a vector, or a small table is close to the solution's.
# Numbers are compared within `tolerance` (absolute). Tables are compared cell
# by cell: rows are matched on the `key` column(s) when given, otherwise by
# position, and the credit is the share of non-key cells that match.
check_value <- function(params, student, reference) {
  obj <- params$object
  tol <- params$tolerance
  key <- unlist(params$key)

  if (!obj %in% names(reference$objects) || is.null(reference$objects[[obj]])) {
    stop("The instructor solution does not create an object named '", obj,
         "', so the rubric cannot check it.", call. = FALSE)
  }
  expected <- reference$objects[[obj]]
  expected_text <- describe_value(expected)
  ref_chunk <- unname(reference$object_chunk[obj])
  ref_chunk <- if (length(ref_chunk) && !is.na(ref_chunk)) ref_chunk else NA_character_

  missing_result <- function(why, observed) {
    check_result(0, paste0("Object `", obj, "` ", why), observed = observed,
                 expected = expected_text, object = obj, chunk = ref_chunk)
  }
  if (!obj %in% names(student$objects)) return(missing_result("was not created.", "(not created)"))
  got <- student$objects[[obj]]
  if (is.null(got)) return(missing_result("was never given a value (it is still NULL).", "(NULL)"))

  chunk <- unname(student$object_chunk[obj])
  chunk <- if (length(chunk) && !is.na(chunk)) chunk else ref_chunk

  res <- if (is.data.frame(expected)) {
    compare_table(got, expected, key, tol)
  } else if (is.numeric(expected)) {
    compare_numeric(got, expected, tol, isTRUE(params$ignore_sign))
  } else {
    compare_other(got, expected)
  }
  check_result(res$credit, res$note, observed = res$observed, expected = expected_text,
               object = obj, chunk = chunk)
}

compare_numeric <- function(got, expected, tol, ignore_sign = FALSE) {
  observed <- describe_value(got)
  if (!is.numeric(got)) {
    return(list(credit = 0, observed = observed,
                note = paste0("Expected a number but got ", class(got)[1], ".")))
  }
  if (length(got) != length(expected)) {
    return(list(credit = 0, observed = observed,
                note = paste0("Expected ", length(expected), " value(s) but got ", length(got), ".")))
  }
  got <- as.numeric(got)
  # With ignore_sign, the answer may be reported for the groups in the other
  # order: a number is negated, and an interval is negated and reversed (a CI of
  # 3 to 9 becomes -9 to -3). The best reading counts.
  candidates <- list(got)
  if (ignore_sign) {
    candidates <- c(candidates, list(if (length(got) > 1) -rev(got) else -got))
  }
  oks <- lapply(candidates, function(g) numbers_match(expected, g, tol))
  ok <- oks[[which.max(vapply(oks, sum, numeric(1)))]]
  credit <- sum(ok) / length(ok)
  note <- if (all(ok)) {
    "Matches the expected value."
  } else if (length(ok) == 1) {
    paste0("Observed ", observed, "; expected ", describe_value(expected), " (tolerance ", format(tol), ").")
  } else {
    paste0(sum(ok), " of ", length(ok), " values match (tolerance ", format(tol), ").")
  }
  list(credit = credit, note = note, observed = observed)
}

compare_other <- function(got, expected) {
  observed <- describe_value(got)
  same <- length(got) == length(expected) &&
    identical(as.character(unlist(got)), as.character(unlist(expected)))
  list(credit = if (same) 1 else 0, observed = observed,
       note = if (same) "Matches the expected value." else
         paste0("Observed ", observed, "; expected ", describe_value(expected), "."))
}

compare_table <- function(got, expected, key, tol) {
  if (is.matrix(got)) got <- as.data.frame(got, stringsAsFactors = FALSE)
  observed <- describe_value(got)
  if (!is.data.frame(got)) {
    return(list(credit = 0, observed = observed,
                note = paste0("Expected a data frame but got ", class(got)[1], ".")))
  }

  expected_cols <- names(expected)
  missing_cols <- setdiff(expected_cols, names(got))
  value_cols <- setdiff(expected_cols, key)

  if (length(key) && any(!key %in% names(got))) {
    return(list(credit = 0, observed = observed,
                note = paste0("The column(s) ", paste0("`", setdiff(key, names(got)), "`", collapse = ", "),
                              " identify the rows but are missing.")))
  }

  if (length(key)) {
    ref_keys <- row_keys(expected, key)
    got_keys <- row_keys(got, key)
    idx <- match(ref_keys, got_keys)
  } else {
    idx <- seq_len(nrow(expected))
    idx[idx > nrow(got)] <- NA_integer_
  }

  if (length(value_cols) == 0) {
    credit <- sum(!is.na(idx)) / max(nrow(expected), 1)
    total <- nrow(expected)
    correct <- sum(!is.na(idx))
  } else {
    total <- nrow(expected) * length(value_cols)
    correct <- 0
    for (cn in intersect(value_cols, names(got))) {
      correct <- correct + sum(cells_match(expected[[cn]], got[[cn]][idx], tol))
    }
    credit <- if (total == 0) 1 else correct / total
  }

  parts <- character()
  if (credit >= 1 - 1e-9 && nrow(got) == nrow(expected)) {
    note <- "Matches the expected table."
  } else {
    parts <- c(parts, paste0(correct, " of ", total, " values match"))
    if (length(missing_cols)) {
      parts <- c(parts, paste0("missing column(s): ", paste0("`", missing_cols, "`", collapse = ", ")))
    }
    if (nrow(got) != nrow(expected)) {
      parts <- c(parts, paste0("expected ", nrow(expected), " rows but got ", nrow(got)))
    }
    note <- paste0(paste(parts, collapse = "; "), ".")
  }
  list(credit = min(credit, 1), note = note, observed = observed)
}

row_keys <- function(df, key) {
  do.call(paste, c(lapply(df[key], function(col) trimws(as.character(col))), sep = "\r"))
}

# Element-wise match of two vectors: numbers within tol (NA matches NA),
# anything else as trimmed text. A shorter `got` (missing rows) is NA.
cells_match <- function(expected, got, tol) {
  if (is.numeric(expected) && is.numeric(got)) return(numbers_match(expected, got, tol))
  e <- trimws(as.character(expected))
  g <- trimws(as.character(got))
  out <- e == g
  out[is.na(out)] <- is.na(e)[is.na(out)] & is.na(g)[is.na(out)]
  out
}

numbers_match <- function(expected, got, tol) {
  out <- abs(expected - got) <= tol + 1e-12
  out[is.na(out)] <- is.na(expected)[is.na(out)] & is.na(got)[is.na(out)]
  out
}

# Short, deterministic text for a value, used in notes and feedback.
describe_value <- function(x) {
  if (is.null(x)) return("(NULL)")
  if (is.data.frame(x)) {
    return(paste0("data frame, ", nrow(x), " row(s), columns: ", paste(names(x), collapse = ", ")))
  }
  if (is.numeric(x) && length(x) <= 5) {
    return(paste(format(x, digits = 6, trim = TRUE), collapse = ", "))
  }
  if (is.character(x) && length(x) <= 5) return(paste0("\"", x, "\"", collapse = ", "))
  paste0(class(x)[1], " of length ", length(x))
}

# --- manual ----------------------------------------------------------------

# Needs human judgment. Not scored; listed for TA review.
check_manual <- function(params, student, reference) {
  check_result(NA_real_, "Needs TA review.", observed = NA_character_, expected = NA_character_)
}
