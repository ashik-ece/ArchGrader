# Defined here rather than relying on base R's `%||%` (R >= 4.4 only).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
