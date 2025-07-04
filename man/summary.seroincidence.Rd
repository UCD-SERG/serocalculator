% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.seroincidence.R
\name{summary.seroincidence}
\alias{summary.seroincidence}
\title{Summarizing fitted seroincidence models}
\usage{
\method{summary}{seroincidence}(object, coverage = 0.95, verbose = TRUE, ...)
}
\arguments{
\item{object}{a \code{\link[=list]{list()}} outputted by \code{\link[stats:nlm]{stats::nlm()}} or \code{\link[=est_seroincidence]{est_seroincidence()}}}

\item{coverage}{desired confidence interval coverage probability}

\item{verbose}{whether to produce verbose messaging}

\item{...}{unused}
}
\value{
a \code{\link[tibble:tibble]{tibble::tibble()}} containing the following:
\itemize{
\item \code{est.start}: the starting guess for incidence rate
\item \code{ageCat}: the age category we are analyzing
\item \code{incidence.rate}: the estimated incidence rate, per person year
\item \code{CI.lwr}: lower limit of confidence interval for incidence rate
\item \code{CI.upr}: upper limit of confidence interval for incidence rate
\item \code{coverage}: coverage probability
\item \code{log.lik}:
log-likelihood of the data used in the call to \code{est_seroincidence()},
evaluated at the maximum-likelihood estimate of lambda
(i.e., at \code{incidence.rate})
\item \code{iterations}: the number of iterations used
\item \code{antigen_isos}: a list of antigen isotypes used in the analysis
\item \code{nlm.convergence.code}:
information about convergence of the likelihood maximization procedure
performed by \code{nlm()}
(see "Value" section of \code{\link[stats:nlm]{stats::nlm()}}, component \code{code});
codes 3-5 indicate issues:
\itemize{
\item 1: relative gradient is close to zero,
current iterate is probably solution.
\item 2: successive iterates within tolerance,
current iterate is probably solution.
\item 3: Last global step failed to locate a point lower than x.
Either x is an approximate local minimum of the function,
the function is too non-linear for this algorithm,
or \code{stepmin} in \code{\link[=est_seroincidence]{est_seroincidence()}}
(a.k.a., \code{steptol} in \code{\link[stats:nlm]{stats::nlm()}}) is too large.
\item 4: iteration limit exceeded; increase \code{iterlim}.
\item 5: maximum step size \code{stepmax} exceeded five consecutive times.
Either the function is unbounded below,
becomes asymptotic to a finite value from above in some direction,
or \code{stepmax} is too small.
}
}
}
\description{
This function is a \code{summary()} method for \code{seroincidence} objects.
}
\examples{

library(dplyr)

xs_data <-
  sees_pop_data_pk_100

curve <-
  typhoid_curves_nostrat_100 |>
  filter(antigen_iso \%in\% c("HlyE_IgA", "HlyE_IgG"))

noise <-
  example_noise_params_pk

est1 <- est_seroincidence(
  pop_data = xs_data,
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

summary(est1)
}
