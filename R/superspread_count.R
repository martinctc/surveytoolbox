#' Convert single-code column(s) into "multiple choice" formats, filling data with sum of counts.
#' 
#' This function counts the number of occurrences per row in a number of categorical / single-code variables.
#' @return n number of new binary / multiple-choice columns / variables, with values representing the count of
#'  row-occurrences in the original set of input categorical/single-code variables.
#'  n represents the number of unique values found in the input variables.
#' 
#' @param df Data frame to apply function to.
#' @param select_helpers Use dplyr select helpers to apply function only to required columns. 
#' Leave blank to include everything
#' @family superspread functions
#' @import dplyr
#' @examples
#' \dontrun{
#' library(data.table)
#' library(dplyr)
#' dt <- data.table(id = 1:10000,
#'                  cat1 = sample(letters[1:3],10000,replace = TRUE),
#'                  cat2 = sample(letters[1:4],10000,replace = TRUE),
#'                  cat3 = sample(letters[1:5],10000,replace = TRUE))
#' dt <- as.data.table(dt)
#' superspread_count(df = dt, select_helpers = contains("cat"))
#' }
#' 
#' @export
superspread_count <- function(df, select_helpers = everything()){
  dt <- data.table(df)
  input_vars_tb <- dplyr::select(df, select_helpers)
  new_dummy_labs <- unique(as.vector(as.matrix(input_vars_tb)))
  
  dt %>%
    .[,(new_dummy_labs) := lapply(new_dummy_labs,
                                  function(x) purrr::reduce(purrr::map(as.list(input_vars_tb), ~.==x),
                                                            `+`))] %>%
    dplyr::as_tibble()
}