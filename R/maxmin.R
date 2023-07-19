#' Max-Min Scaling Function
#'
#' This function allows you to scale vectors or an entire data frame using the max-min scaling method
#' A numeric vector is always returned.
#' @param x Pass a vector or the required columns of a data frame through this argument.
#' @keywords max-min
#' @export
#' @examples
#' library(magrittr)
#' library(dplyr)
#' 
#' 
#' rand.data <- data.frame(x = sample(x = 1000, size = 234:697))
#' rand.data %>%
#'   mutate(
#'     y = maxmin(x)
#'   )
#' 
#' sample(x = 1000, size = 234:677) %>% maxmin()
#' 
#' iris %>% mutate(Petal.Length2 = maxmin(Petal.Length))
#' 
#' maxmin(iris$Petal.Length)
#' @export
maxmin <- function(x){
    if(any(is.na(x))){
      warning("Warning: vector contains missing values. Those values will return as NA.")
    }
    maxs <- max(x, na.rm = TRUE)
    mins <- min(x, na.rm = TRUE)
    as.numeric(scale(x,center=mins,scale=maxs-mins))
}
