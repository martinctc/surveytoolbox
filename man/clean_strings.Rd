% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_strings.R
\name{clean_strings}
\alias{clean_strings}
\title{Clean strings so that they can be used as variable names or column names}
\usage{
clean_strings(string, treat_dups = TRUE)
}
\arguments{
\item{string}{Character string to be "cleaned".}

\item{treat_dups}{Set to FALSE to allow duplication of strings. Defaults to TRUE}
}
\value{
A "cleaned" string character vector.
}
\description{
Clean strings so that they can be used as variable names or column names
}
\examples{
clean_strings(c("Respondent ID","Q23. Brand Awareness"))
}
