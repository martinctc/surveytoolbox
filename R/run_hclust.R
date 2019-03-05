#' A wrapper function to run hierarchical clustering
#' 
#' @param x Data frame to run hierarchical clustering
#' @param method Specify method for hierarchical clustering. See ?hclust for details
#' @param dmethod Specify method for calculating distance. See ?dist for details
#' @examples 
#' library(magrittr)
#' library(dplyr)
#' iris %>% select(-Species) %>% run_hclust()
#' @export
run_hclust <- function(x, method = "complete", dmeth = "euclidean") {    
  hclust(dist(x, method = dmeth), method = method)
}
