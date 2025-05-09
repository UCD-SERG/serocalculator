% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/warn_missing_strata.R
\name{warn_missing_strata}
\alias{warn_missing_strata}
\title{Warn about missing stratifying variables in a dataset}
\usage{
warn_missing_strata(data, strata, dataname)
}
\arguments{
\item{data}{the dataset that should contain the strata}

\item{strata}{a \code{\link[=data.frame]{data.frame()}} showing the strata levels
that are expected to be in the dataset}

\item{dataname}{the name of the dataset,
for use in warning messages if some strata are missing.}
}
\value{
a \code{\link[=character]{character()}} vector of the subset of stratifying variables
that are present in \code{pop_data}
}
\description{
Warn about missing stratifying variables in a dataset
}
\examples{
\dontrun{
expected_strata <- data.frame(Species = "banana", type = "orchid")

warn_missing_strata(iris, expected_strata, dataname = "iris")
}
}
\keyword{internal}
