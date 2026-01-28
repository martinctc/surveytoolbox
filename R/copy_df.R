#' Copy a data frame to clipboard for pasting in Excel
#' 
#' This is a pipe-optimised function, and
#' accompanies read_df() as a tool for ad-hoc analysis, which reads a data table copied from Excel into R.
#' 
#' @note This function only works on Windows. On other platforms, it will
#'   issue a warning and return the input invisibly.
#'
#' @param x Data frame to be passed through. Cannot contain list-columns or nested data frames.
#' @param row.names A logical vector for specifying whether to allow row names. Defaults to FALSE.
#' @param col.names A logical vector for specifying whether to allow column names. Defaults to FALSE.
#' @param expand Add number to manually expand clipboard size
#' @param quietly Set this to TRUE to not print data frame on console
#' @param ... Additional arguments for write.table().
#' 
#' @return Invisibly returns the input data frame `x`.
#' 
#' @export
copy_df <- function(x, row.names = FALSE,
                    col.names = TRUE,
                    expand = "", quietly = FALSE, ...) {
  
 if (.Platform$OS.type != "windows") {
    warning("copy_df() only works on Windows.", call. = FALSE)
    return(invisible(x))
  }
  
  expand_x <- stringr::str_remove_all(expand, "-") # For backward compatibility
  if (expand == "") {
    write.table(x, "clipboard-5000",
                sep = "\t",
                row.names = row.names,
                col.names = col.names, ...)
  } else {
    expand_x <- paste0("-", expand_x)
    write.table(x, paste0("clipboard", expand_x),
                sep = "\t",
                row.names = row.names,
                col.names = col.names,
                ...)
  }
  if (quietly == FALSE) print(x)
  invisible(x)
}
