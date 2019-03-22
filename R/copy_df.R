#' Copy a data frame to clipboard for pasting in Excel
#' 
#' This is a pipe-optimised function, and
#' accompanies read_df() as a tool for ad-hoc analysis, which reads a data table copied from Excel into R.
#'
#' @param x Data frame to be passed through. Cannot contain list-columns or nested data frames.
#' @param row.names A logical vector for specifying whether to allow row names. Defaults to FALSE.
#' @param col.names A logical vector for specifying whether to allow column names. Defaults to FALSE.
#' @param expand Add number to manually expand clipboard size
#' @param quietly Set this to TRUE to not print data frame on console
#' @param ... Additional arguments for write.table().
#' @export
copy_df <-function(x,row.names=FALSE,col.names=TRUE,expand="",quietly=FALSE,...) {
  expand_x <- stringr::str_remove_all(expand,"-") # For backward compatibility
  if(expand==""){
    write.table(x,"clipboard-5000",sep="\t",row.names=row.names,col.names=col.names,...)
  } else {
    expand_x <- paste0("-",expand_x)
    write.table(x,paste0("clipboard",expand_x),sep="\t",row.names=row.names,col.names=col.names,...)
  }
  if(quietly==FALSE) print(x)
}
