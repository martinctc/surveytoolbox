#' Calculate (simple) weights based on the proportion of another variable.
#' 
#' @param data Data frame or tibble containing the variable used for computing weights
#' @param weight_var The variable to be used for computing the weight 
#' @param target A named vector containing the target weights. Names must mtach labels for `weight_var`
#' @param weight_label A character vector to specify the variable name of the weight variable. Defaults to 'weight'.
#' 
#' @export
calc_weights <- function(data, weight_var, target, weight_label = "weight"){
  if(anyNA(data[[weight_var]])){
    stop("`weight_var` contains missing values. Please impute or remove!")
  } else {
    data %>%
      count(!!sym(weight_var)) %>%
      mutate(prop = n / sum(n, na.rm = TRUE)) -> count_tb
    
    target %>% enframe(name = weight_var) -> weight_target
    
    count_tb %>%
      left_join(weight_target, by = weight_var) %>%
      mutate(!!sym(weight_label) := value / prop) %>%
      select(weight_var, weight_label) -> weight_results
    
    data %>%
      left_join(weight_results, by = weight_var)
  }
}

# tibble(cat = sample(c("A", "B", "C"), 1000, replace = TRUE, prob = c(.5,.2,.3))) -> df
# df[3,] <- NA
# calc_weights(df, "cat", c("A" = 1/3, "B" = 1/3, "C" = 1/3))


