#' Convert correlation matrix into a tidy data frame
#' Use label_table as an input
#' Identify matching id
#' 
#' @param cor_m Correlation matrix in the form of an output from cor()
#' @param label_table A two-column table with one column used for matching and the other for returning labels.
#' @param id Use this argument to specify a string to refers to the matching / id column in the label_table.
#' 
cor_to_df <- function(cor_m, label_table, id){
  if(ncol(label_table)!=2){
    stop("Matching table must have an id column and a return column.")
  } else {
    matched_lab <- names(label_table)[names(label_table)!=id]
    
    cor_m %>%
      as.data.frame() %>%
      tibble::rownames_to_column("cor_matrix") %>%
      dplyr::left_join(label_table,by=c("cor_matrix"=id)) %>%
      dplyr::select(cor_matrix,matched_lab,everything())-> output_df
    
    labels <- output_df[[matched_lab]]
    
    names(output_df) <- c("cor_matrix",matched_lab,labels)
    return(output_df)
  }
}