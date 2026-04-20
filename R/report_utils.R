#' Coerce TAC Column Name
#'
#' @description
#' Standardize TAC column naming across report pipelines where source files may
#' contain either `TAC` or `tac` (or another explicitly provided candidate).
#'
#' @param data A data.frame or tibble.
#' @param target_col Character string giving the desired output column name.
#' @param candidates Character vector of possible source column names to use in
#'   priority order. Defaults to `c("tac", "TAC")`.
#'
#' @return Input `data` with the selected TAC column renamed to `target_col`
#'   (unless `target_col` already exists, in which case data is returned
#'   unchanged).
#' @export
coerce_tac_column_name <- function(data, target_col = "tac", candidates = c("tac", "TAC")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }

  if (!is.character(target_col) || length(target_col) != 1L || target_col == "") {
    stop("`target_col` must be a single non-empty character string.")
  }

  if (target_col %in% names(data)) {
    return(data)
  }

  matched_source <- candidates[candidates %in% names(data)][1]

  if (is.na(matched_source) || length(matched_source) == 0) {
    stop(
      "Could not find any TAC column to rename. Tried: ",
      paste(candidates, collapse = ", "),
      ". Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }

  dplyr::rename(data, !!target_col := !!rlang::sym(matched_source))
}
