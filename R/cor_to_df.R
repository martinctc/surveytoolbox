#' Convert correlation matrix into a tidy data frame
#' 
#' This function assumes that the input into cor() uses variable names, and available
#' variable labels are not used.
#' If either of the `label_table` or `id` arguments are unused, `cor_to_df`
#' simply returns a tibble without label matching.
#' Use the `label_table` and `id` arguments to match up variable names
#'  with the variable labels.
#' 
#' @param cor_m Correlation matrix in the form of an output from cor().
#' @param label_table A two-column table with one column used for matching and the other for returning labels.
#' @param id A character vector specifying the name of the matching / id column in the label_table.
#' @export
cor_to_df <- function(cor_m, label_table = NULL, id = NULL){
  if(is.null(label_table) | is.null(id)){
    message("Returning a tibble without changing labels")
    cor_m %>%
      tibble::as_tibble(rownames = "cor_matrix") %>%
      return()
  } else {
    if(ncol(label_table)!=2){
      stop("Matching table must have an id column and a return column.")
    } else {
      matched_lab <- names(label_table)[names(label_table)!=id]
      
      cor_m %>%
        tibble::as_tibble(rownames = "cor_matrix") %>%
        dplyr::left_join(label_table,by=c("cor_matrix"=id)) %>%
        dplyr::select(cor_matrix,matched_lab,everything())-> output_df
      
      labels <- output_df[[matched_lab]]
      
      names(output_df) <- c("cor_matrix",matched_lab,labels)
      return(output_df)
    }
  }
}
