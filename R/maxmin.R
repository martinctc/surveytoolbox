#' Max-Min Scaling Function
#'
#' This function allows you to scale vectors or an entire data frame using the max-min scaling method,
#' always returning a data frame. 
#' @param x Pass a vector or the required columns of a data frame through this argument.
#' @keywords max-min
#' @export
#' @examples
#' rand.data <-cbind(sample(1000,234:697),sample(1000,234:697)) %>% as.data.frame()
#' maxmin(rand.data)
#' 
#' rand.data <-sample(1000,234:677)
#' maxmin(rand.data)

maxmin <- function(x){
  if(is.vector(x)==TRUE){
    maxs <- max(x)
    mins <- min(x)
    scale(x,center=mins,scale=maxs-mins) %>%
      as.data.frame()
  } else {
    maxs <- apply(x, 2,max)
    mins <- apply(x, 2,min)
    scale(x, center = mins, scale = maxs - mins) %>%
      as.data.frame()    
  }
}