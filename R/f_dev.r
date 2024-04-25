#  Utility functions: interface with C lib serocalc.so

#' Calculate negative log-likelihood (deviance)
#'
#' more description to be added here
#' @param lambda incidence parameter, in events per person-year

#' @param csdata cross-sectional sample data containing variables `value` and `age`
#' @param lnpars longitudinal antibody decay model parameters `alpha`, `y1`, and `d`
#' @param cond measurement noise parameters `nu`, `eps`, `y.low`, and `y.high`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `fdev()` was renamed to `f_dev()` to create a more
#' consistent API.
#'
#' @keywords internal
#'
#' @export
fdev <- Vectorize(
  vectorize.args = "lambda",
  function(
    lambda,
    csdata,
    lnpars,
    cond)
  {
    lifecycle::deprecate_warn("1.0.0", "fdev()", "f_dev()")
    f_dev(lambda, csdata, lnpars, cond)
  })

#  Utility functions: interface with C lib serocalc.so

#' Calculate negative log-likelihood (deviance)
#'
#' more description to be added here
#' @param lambda incidence parameter, in events per person-year

#' @param csdata cross-sectional sample data containing variables `value` and `age`
#' @param lnpars longitudinal antibody decay model parameters `alpha`, `y1`, and `d`
#' @param cond measurement noise parameters `nu`, `eps`, `y.low`, and `y.high`
#' @export
f_dev <- Vectorize(
  vectorize.args = "lambda",
  function(
    lambda,
    csdata,
    lnpars,
    cond)
  {

    res <- 0;
    lambda <- as.double(lambda);
    y <- as.double(csdata$"value");
    a <- as.double(csdata$"age");
    nsubj <- as.integer(nrow(csdata));
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
  })
