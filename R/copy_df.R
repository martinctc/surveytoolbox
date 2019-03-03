#' Copy a data frame to clipboard for pasting in Excel
#' Pipe optimised
#' @param expand Add number to manually expand clipboard size
#' @param quietly Set this to TRUE to not print data frame on console
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
