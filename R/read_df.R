#' Read in a data frame from the clipboard, copied from Excel
#' 
#' @note This function only works on Windows. On other platforms, it will
#'   stop with an error.
#' 
#' @param header Logical value to specify whether copied table contains a header row.
#' @param ... Additional arguments for read.table().
#' 
#' @return A data frame containing the clipboard contents.
#' 
#' @export
read_df <- function(header = TRUE, ...) {
  if (.Platform$OS.type != "windows") {
    stop("read_df() only works on Windows.", call. = FALSE)
  }
  read.table("clipboard", sep = "\t", header = header, ...)
}
