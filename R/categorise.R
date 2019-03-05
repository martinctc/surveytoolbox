#' Convert numeric variable into categorical variable
#' Essentially a wrapper around the cut() function
#' 
#' @param x vector to be passed through
#' @param break points for the categories. Both the top and bottom breaks must be provided.
#' @examples 
#' categorise(seq(10),breaks=c(0,3,10))
#' @export
categorise <- function(x, breaks){
  x %>%
    cut(breaks=breaks,
        include.lowest=TRUE,
        right=TRUE) %>%
    unlist() %>% as.character()
}