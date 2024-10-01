#' @title Create frequency distribution table for a metric
#'
#' @description This function creates a frequency distribution table for a given
#'   metric. The table contains the bin ranges and the counts of the data points
#'   that fall within each bin.
#'
#' @param data A data frame containing the data
#' @param metric string specifying the name of the metric for which the
#'   frequency distribution is to be created
#' 
#' @examples
#' create_freq_dist(iris, "Sepal.Length")
#' 
#' @export
create_freq_dist <- function(data, metric){
  
  hist_data <- hist(data[[metric]], plot = FALSE)
  
  # Create labels for the bin ranges
  bin_labels <- paste0(
    hist_data$breaks[-length(hist_data$breaks)], 
    " - ", 
    hist_data$breaks[-1]
  )
  
  hist_df <- data.frame(
    metric = metric,
    bin_range = bin_labels,
    counts = hist_data$counts
  )
  
  return(hist_df)
}