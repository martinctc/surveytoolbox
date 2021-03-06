% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/any_x.R
\name{any_x}
\alias{any_x}
\title{Function that returns TRUE/FALSE if value exists in x, but returns NA if x consists entirely of NAs}
\usage{
any_x(x, value)
}
\arguments{
\item{x}{Vector of values to test.}

\item{value}{Value to test whether it exists in x. NA is returned if none exists at all.}
}
\value{
A logical vector whether a value exists in x, and returns NA if x contains only NAs.
}
\description{
A more nuanced response is returned than the standard R method,
which does not return NAs if x is all NAs.
Has useful applications in understanding a set of categorical variables
belonging to a single question.
E.g. A question on brand usage across 10 product types to understand 'any' usage of a brand x.
}
\examples{
any_x(c(1,0,1),1) # TRUE
any_x(c(1,NA,1),1) # TRUE
any_x(c(0,0,NA),1) # FALSE
any_x(c(NA,NA,NA),1) # NA

}
