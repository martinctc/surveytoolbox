#' @title 
#' Compute chi-square or Fisher's exact test for two categorical variables
#'
#' @description 
#' This function computes a chi-square or Fisher's exact test for two categorical variables in a data frame.
#'
#' @param data A data frame containing the variables of interest.
#' @param x A character string specifying the name of the first variable.
#' @param y A character string specifying the name of the second variable.
#' @param na_x A vector of values to be treated as missing in \code{x}.
#' @param na_y A vector of values to be treated as missing in \code{y}.
#' 
#' @details 
#' If the expected cell counts are lower than 5, the function will use Fisher's exact test. Otherwise, it will use a chi-square test.
#'
#' @return A tibble containing the results of the chi-square or Fisher's exact test.
#'
#' @examples
#' data("mtcars")
#' test_chisq(mtcars, "cyl", "vs")
#'
#' @importFrom rstatix chisq_test
#' @importFrom stats fisher.test
#' @importFrom dplyr filter mutate select
#' @importFrom broom tidy
#'
#' @export
test_chisq <- function(data, x, y, na_x = NULL, na_y = NULL){
  
  # Remove missing values, including any user-specified missing codes
  data2 <-
    data %>%
    filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
    filter(!.data[[x]] %in% na_x, !.data[[y]] %in% na_y)
  
  # Create new variables to feed into `rstatix::chisq_test()`
  stat_x <- data2[[x]]
  stat_y <- data2[[y]]
  
  # Check expected cell counts
  expected_counts <-
    suppressWarnings(
      chisq.test(table(stat_x, stat_y))$expected
    )

  if (any(expected_counts < 5)) {
    # Use Fisher's exact test if expected cell counts are low
    result <- fisher.test(x = factor(stat_x), y = factor(stat_y)) %>%
      broom::tidy() %>% # Return a data frame
      mutate(n = nrow(data2),
             statistic = NA_real_,
             df = NA_integer_,
             p.signif = NA_character_,
             p = .data$p.value) %>%
      select(
        n, 
        statistic,
        p, 
        df,
        method, 
        p.signif,
        alternative
      )
    
  } else {
    # Use chi-square test if expected cell counts are not low
    result <- rstatix::chisq_test(x = stat_x, y = stat_y) %>%
      mutate(alternative = NULL)
  }
  
  # Return results
  tibble::tibble(
    col_x = x,
    col_y = y
  ) %>%
    cbind(result) %>%
    dplyr::as_tibble()
  
}
