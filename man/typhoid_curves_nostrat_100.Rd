% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_typhoid_curves_nostrat.R
\docType{data}
\name{typhoid_curves_nostrat_100}
\alias{typhoid_curves_nostrat_100}
\title{Small example of antibody response curve parameters for typhoid}
\format{
\subsection{\code{typhoid_curves_nostrat_100}}{

A \code{curve_params} object (from \code{\link[=as_sr_params]{as_sr_params()}}) with 500 rows and 7
columns:
\describe{
\item{antigen_iso}{which antigen and isotype are being measured
(data is in long format)}
\item{iter}{MCMC iteration}
\item{y0}{Antibody concentration at t = 0 (start of active infection)}
\item{y1}{Antibody concentration at t = \code{t1} (end of active infection)}
\item{t1}{Duration of active infection}
\item{alpha}{Antibody decay rate coefficient}
\item{r}{Antibody decay rate exponent parameter}
}
}
}
\source{
\url{https://osf.io/rtw5k}
}
\usage{
typhoid_curves_nostrat_100
}
\description{
A subset of data from the SEES study, for examples and testing.
}
\keyword{datasets}
