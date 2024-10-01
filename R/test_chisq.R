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
#' If the cell counts are lower than 5, the function will use Fisher's exact test. Otherwise, it will use a chi-square test.
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
#' @importFrom tidyr pivot_longer
#' @importFrom broom tidy
#'
#' @export
test_chisq <- function(data, x, y, na_x = NULL, na_y = NULL){
  
  # remove NA values
  data2 <- 
    data %>%
    filter(!(!!sym(x) %in% na_x)) %>%
    filter(!(!!sym(y) %in% na_y))
  
  # Create new variables to feed into `rstatix::chisq_test()`
  stat_x <- data2[[x]]
  stat_y <- data2[[y]]
  
  # Check expected cell counts
  expected_counts <-
    chisq.test(table(data2[[x]], data2[[y]]))$expected %>%
    suppressWarnings()
  
  if (any(expected_counts < 5)) {
    # Use Fisher's exact test if expected cell counts are low
    result <- fisher.test(x = factor(stat_x), y = factor(stat_y)) %>%
      broom::tidy(out) %>% # Return a data frame
      mutate(n = NA,
             statistic = NA,
             df = NA,
             `p.signif` = NA,
             p = `p.value`) %>%
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
  dplyr::tibble(
    col_x = x,
    col_y = y
  ) %>%
    cbind(result) %>%
    dplyr::as_tibble()
  
}