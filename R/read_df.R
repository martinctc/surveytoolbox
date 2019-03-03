#' Read in a data frame in the clipboard, copied from Excel
#' 
#' @param header Logical value to specify whether copied table contains a header row.
#' @export
read_df <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}
