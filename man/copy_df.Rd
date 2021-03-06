% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy_df.R
\name{copy_df}
\alias{copy_df}
\title{Copy a data frame to clipboard for pasting in Excel}
\usage{
copy_df(
  x,
  row.names = FALSE,
  col.names = TRUE,
  expand = "",
  quietly = FALSE,
  ...
)
}
\arguments{
\item{x}{Data frame to be passed through. Cannot contain list-columns or nested data frames.}

\item{row.names}{A logical vector for specifying whether to allow row names. Defaults to FALSE.}

\item{col.names}{A logical vector for specifying whether to allow column names. Defaults to FALSE.}

\item{expand}{Add number to manually expand clipboard size}

\item{quietly}{Set this to TRUE to not print data frame on console}

\item{...}{Additional arguments for write.table().}
}
\description{
This is a pipe-optimised function, and
accompanies read_df() as a tool for ad-hoc analysis, which reads a data table copied from Excel into R.
}
