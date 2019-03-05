#' Convert single-code column(s) into "multiple choice" formats, filling data from a target column
#' 
#' @param df Data frame to apply function to.
#' @param tcol Number of columns to create
#' @param select_helpers Use dplyr select helpers to apply function only to required columns. 
#' @param target_col Target column to take values from, in order to fill the new columns
#' Leave blank to include everything
#' 
#' @examples 
#' library(dplyr)
#' library(magrittr)
#' data.frame(a=c(1,2,4,5,6,7,8),b=c(244,333,434,453,123,123,435)) %>%
#'  superspread_fill(8,"a","b")
#' 
#' @export
superspread_fill <- function(df,tcol,select_helpers=everything(),target_col){
  df_original <- df
  df <- dplyr::select(df,select_helpers)
  dft <- dplyr::select(df_original,target_col)
  
  frameNA <-matrix(NA,nrow(df),tcol) %>% as.data.frame()
  names(frameNA) <-seq(1,tcol)
  
  for (i in 1:tcol){ # For each column
    frameNA[,i] <- as.numeric(sapply(1:nrow(df), function(x) ifelse(i %in% df[x,],
                                                                    sapply(x,function(y) dft[y,1]),
                                                                    0))) 
  }
  cbind(df,frameNA)
}