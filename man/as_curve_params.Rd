% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_curve_params.R
\name{as_curve_params}
\alias{as_curve_params}
\title{Load antibody decay curve parameter}
\usage{
as_curve_params(data, antigen_isos = NULL)
}
\arguments{
\item{data}{a \code{\link[=data.frame]{data.frame()}} or \link[tibble:tbl_df-class]{tibble::tbl_df}}

\item{antigen_isos}{a \code{\link[=character]{character()}} vector of antigen isotypes
to be used in analyses}
}
\value{
a \code{curve_data} object
(a \link[tibble:tbl_df-class]{tibble::tbl_df} with extra attribute \code{antigen_isos})
}
\description{
Load antibody decay curve parameter
}
\examples{
library(magrittr)
curve_data <-
  "https://osf.io/download/rtw5k/" \%>\%
  readr::read_rds() \%>\%
  as_curve_params()

print(curve_data)
}
