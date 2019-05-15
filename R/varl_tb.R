#' Create tidy data frame with variable and variable labels
#' 
#' Input a data frame or tibble containing labelled variables,
#' and returns a tidy tibble with variable and variable labels.
#' Empty variables will be imputed with "No label"
#' 
#' @param x Data frame containing labelled variables to pass through
#' 
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr is_null
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' 
#' @export
varl_tb <- function(x){
  purrr::map(x, ~attr(.,'label')) %>%
    purrr::map(~ifelse(purrr::is_null(.),"No label", .)) %>%
    tibble::enframe(name = "var", value = "var_label") %>%
    tidyr::unnest()
}
