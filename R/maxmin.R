#' Max-Min Scaling Function
#'
#' This function allows you to scale vectors or an entire data frame using the max-min scaling method
#' A numeric vector is always returned.
#' @param x Pass a vector or the required columns of a data frame through this argument.
#' @keywords max-min
#' @export
#' @examples
#' rand.data <-cbind(sample(1000,234:697),sample(1000,234:697)) %>% as.data.frame()
#' rand.data %>% mutate_all(~maxmin(.))
#' 
#' sample(1000,234:677) %>% maxmin()
#' 
#' iris %>% mutate_at(vars(Petal.Length),~maxmin(.))
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
