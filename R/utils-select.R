# Internal helper for functions that accept either tidyselect helpers
# (e.g. `everything()`, `starts_with()`) or a character vector of column names.
# Callers should pass a quosure, e.g. `select_cols(data, rlang::enquo(cols))`.
# Using `all_of()` for character vectors avoids the tidyselect deprecation
# warning about external vectors.
select_cols <- function(data, cols_quo) {
  if (rlang::quo_is_missing(cols_quo)) {
    return(data)
  }

  expr <- rlang::quo_get_expr(cols_quo)

  # A symbol may point to a character vector supplied by the caller.
  if (rlang::is_symbol(expr)) {
    value <- tryCatch(
      rlang::eval_tidy(cols_quo),
      error = function(e) NULL
    )

    if (is.character(value)) {
      return(dplyr::select(data, dplyr::all_of(value)))
    }
  }

  if (is.character(expr)) {
    return(dplyr::select(data, dplyr::all_of(expr)))
  }

  dplyr::select(data, !!cols_quo)
}
