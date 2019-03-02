#' Run hierarchical clustering with arguments to specify methods.
#' 

hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {    
  hclust(dist(x, method = dmeth), method = method)
}