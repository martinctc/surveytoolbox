#' Convert single-code column(s) into "multiple choice" formats - fill data with 1s
#' 
#' This function was orignally used in the context of a 'Select Any' question, where the outputs were created such that each 'cell' value represents the statement, rather than having the columns represent the statements. 
#' tcol refers to the number of columns to create. 
#' 'Cell' values must be in numeric. 
#' Version with select incorporated                                    
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
#' superspread(df,50)
#' @export
superspread <- function(df,tcol,select_helpers=everything()){
  df_original <- df
  df <- dplyr::select(df,select_helpers)
  frameNA <-matrix(NA,nrow(df),tcol) %>% as.data.frame()
  names(frameNA) <-seq(1,tcol)
  for (i in 1:tcol){
    frameNA[,i] <- as.numeric(apply(df, 1, function(x) ifelse(i %in% x, 1, 0)))
  }
  cbind(df_original,frameNA)
}    
