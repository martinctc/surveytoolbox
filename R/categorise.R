#' Convert numeric variable into categorical variable
#' 
#' This is a wrapper around the cut() function.
#' 
#' @return Returns a character vector. 
#' @param x A numeric vector to be passed through.
#' @param breaks Break points for the categories. Both the top and bottom breaks must be provided.
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