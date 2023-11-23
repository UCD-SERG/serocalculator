#  Utility functions: interface with C lib serocalc.so

#' Calculate negative log-likelihood (deviance)
#'
#' more description to be added here
#' @param log.lambda natural logarithm of incidence parameter, in log(events per person-year).
#' * Value of -6 corresponds roughly to 1 day (`log(1/365.25)`)
#' * Value of -4 corresponds roughly to 1 week (`log(7 / 365.25)`).
#' @param lambda incidence parameter, in events per person-year

#' @param csdata cross-sectional sample data containing variables `y` and `a`
#' @param lnpars longitudinal antibody decay model parameters `alpha`, `y1`, and `d`
#' @param cond measurement noise parameters `nu`, `eps`, `y.low`, and `y.high`
#' @export

fdev <- function(
    log.lambda = log(lambda),
    lambda = exp(log.lambda),
    csdata,
    lnpars,
    cond)
{
  res <- 0;
  lambda <- as.double(exp(log.lambda));
  y <- as.double(csdata$y);
  a <- as.double(csdata$a);
  nsubj <- as.integer(length(y));
  y1 <- as.double(lnpars$y1);
  alpha <- as.double(lnpars$alpha);
  d <- as.double(lnpars$d);
  nmc <- as.integer(length(y1));
  step <- as.double(max(y1)/100); # hack for numerical integrations
  nu <- as.double(cond$nu);
  eps <- as.double(cond$eps);
  y.low <- as.double(cond$y.low);
  y.high <- as.double(cond$y.high);
  llpp <- .C(
    "negloglik",
    res=as.double(res),
    lambda=lambda,
    y=y,
    a=a,
    nsubj=nsubj,
    nu=nu,
    eps=eps,
    step=step,
    y.low=y.low,
    y.high=y.high,
    y1=y1,
    alpha=alpha,
    d=d,
    nmc=nmc);
  return(llpp$res);
}

