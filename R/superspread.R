#' Creates dummy variables from multiple categorical variables.
#' Uses data.table() for speed (enhanced from the previous version)
#' Uses dplyr select() special functions to select categorical variables.
#' 
#' @param df Data frame containing the multiple categorical variables.
#' @param select_helpers Uses dplyr-style select functions to select multiple variables. 
#' Use everything() to select all variables. These variables must all be character type.
#' @family superspread functions
#' @import dplyr
#' 
#' @examples 
#' \dontrun{
#' library(data.table)
#' library(dplyr)
#' dt <- data.table(id = 1:10000,
#'                  cat1 = sample(letters[1:3],10000,replace = TRUE),
#'                  cat2 = sample(letters[1:4],10000,replace = TRUE),
#'                  cat3 = sample(letters[1:5],10000,replace = TRUE))
#' dt <- as.data.table(dt)
#' superspread(df = dt, select_helpers = contains("cat"))
#' }
#' 
#' @export
superspread <- function (df, select_helpers) {
  df <- data.table::data.table(df)
  input_vars_tb <- dplyr::select(df, select_helpers)
  new_dummy_labs <- unique(as.vector(as.matrix(input_vars_tb)))
  pairwise_any <- function(var1, var2) {
    if (length(var1) != length(var2)) {
      stop("Vectors are of different lengths!")
    }
    else {
      sapply(1:length(var1), function(x) any(var1[x], var2[x]))
    }
  }
  df %>%
    .[, ":="((new_dummy_labs),
             lapply(new_dummy_labs,
                    function(x) purrr::reduce(purrr::map(as.list(input_vars_tb),
                                                         ~. %in% x), pairwise_any)))] %>%
    dplyr::as_tibble()
}

