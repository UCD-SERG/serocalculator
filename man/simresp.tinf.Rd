% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simresp.tinf.R
\name{simresp.tinf}
\alias{simresp.tinf}
\title{simulate antibody kinetics of y over a time interval}
\usage{
simresp.tinf(
  lambda,
  t_end,
  age_fixed,
  antigen_isos,
  n_mcmc_samples = 0,
  renew_params,
  predpar,
  ...
)
}
\arguments{
\item{lambda}{seroconversion rate (1/days),}

\item{t_end}{end of time interval (beginning is time 0) in days(?)}

\item{age_fixed}{parameter estimates for fixed age (age_fixed in years) or not.
when age_fixed = NA then age at infection is used.}

\item{antigen_isos}{antigen isotypes}

\item{n_mcmc_samples}{a posterior sample may be selected (1:4000), or not
when n_mcmc_samples = 0 a posterior sample is chosen at random.}

\item{renew_params}{At infection,
a new parameter sample may be generated
(when \code{renew_params = TRUE}).
Otherwise (when \code{renew_params = FALSE}),
a sample is generated at birth and kept,
but baseline y0 are carried over from prior infections.}

\item{predpar}{an \code{\link[=array]{array()}} with dimensions named:
\itemize{
\item \code{antigen_iso}
\item \code{parameter}
\item \code{obs}
}}

\item{...}{
  Arguments passed on to \code{\link[=ldpar]{ldpar}}, \code{\link[=ab]{ab}}, \code{\link[=mk_baseline]{mk_baseline}}
  \describe{
    \item{\code{age}}{age at infection}
    \item{\code{nmc}}{mcmc sample to use}
    \item{\code{npar}}{number of parameters}
    \item{\code{t}}{\link{numeric} \link{vector} of elapsed times since start of infection}
    \item{\code{par}}{\link{numeric} \link{matrix} of model parameters:
\itemize{
\item rows are parameters
\item columns are biomarkers
}}
    \item{\code{kab}}{\link{integer} indicating which row to read from \code{blims}}
    \item{\code{n}}{number of observations}
    \item{\code{blims}}{range of possible baseline antibody levels}
  }}
}
\value{
a \link{list} with:
\itemize{
\item t = times (in days, birth at day 0),
\item b = bacteria level, for each antibody signal
(not used; probably meaningless),
\item y = antibody level, for each antibody signal
\item smp = whether an infection involves a big jump or a small jump
\item t.inf = times when infections have occurred.
}
}
\description{
simulate antibody kinetics of y over a time interval
}
\keyword{internal}
