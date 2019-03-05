#' Convert single-code column(s) into "multiple choice" formats, filling data with count rather than 1s
#' 
#' @param df Data frame to apply function to.
#' @param tcol Number of columns to create
#' @param select_helpers Use dplyr select helpers to apply function only to required columns. 
#' Leave blank to include everything
#' 
#' @examples
#' library(dplyr)
#' library(magrittr)
#' df <- data.frame(x=sample(1:50,1000,replace=TRUE))
#' superspread_count(df,50)
#' 
#' @export
superspread_count <- function(df,tcol,select_helpers=everything()){  
  df_original <- df
  df <- select(df,select_helpers)
  
  frameNA <-matrix(NA,nrow(df),tcol) %>% as.data.frame()
  names(frameNA) <-seq(1,tcol)
  for (i in 1:tcol){
    frameNA[,i] <- as.numeric(apply(df, 1, function(x) ifelse(i %in% x, sum(i==x,na.rm = TRUE), 0))) # count if 'column value' exists in the matrix
  }
  
  names(frameNA) <- paste0("x",names(frameNA))
  
  cbind(df_original,frameNA)
}