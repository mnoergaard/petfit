# Input validation functions
# Validation logic for user inputs and configuration parameters

#' Coerce Parameter Bounds to Numeric
#'
#' @description Recursively convert 'start', 'lower', and 'upper' values to numeric
#' in a config list. This fixes the jsonlite integer coercion issue where whole
#' numbers like 5 become integer class instead of numeric, causing minpack.lm to fail.
#' Only targets bound-related fields, preserving integer values for other fields
#' like multstart_iter.
#'
#' @param x A list, vector, or atomic value from JSON config
#' @return The same structure with start/lower/upper values converted to numeric
#' @export
coerce_bounds_numeric <- function(x) {
  if (is.list(x)) {
    # Check for named lists with start/lower/upper keys
    if (!is.null(names(x))) {
      bound_keys <- c("start", "lower", "upper")
      for (key in bound_keys) {
        if (key %in% names(x) && is.numeric(x[[key]])) {
          x[[key]] <- as.numeric(x[[key]])
        }
      }
    }
    # Recurse into all list elements
    lapply(x, coerce_bounds_numeric)
  } else {
    x
  }
}