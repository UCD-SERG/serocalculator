#  Utility functions: interface with C lib serocalc.so

#' Calculate negative log-likelihood (deviance)
#'
#' more description to be added here
#' @param log.lambda Initial guess of incidence rate
#' @param csdata cross-sectional sample data
#' @param lnpars longitudinal antibody decay model parameters
#' @param cond measurement noise parameters
#' @export

fdev <- function(log.lambda,csdata,lnpars,cond)
{
  res <- 0; lambda <- as.double(exp(log.lambda));
  y <- as.double(csdata$y); a <- as.double(csdata$a);
  nsubj <- as.integer(length(y));
  y1 <- as.double(lnpars$y1); alpha <- as.double(lnpars$alpha);
  d <- as.double(lnpars$d); nmc <- as.integer(length(y1));
  step <- as.double(max(y1)/100); # hack for numerical integrations
  nu <- as.double(cond$nu); eps <- as.double(cond$eps);
  y.low <- as.double(cond$y.low); y.high <- as.double(cond$y.high);
  llpp <- .C("negloglik",res=as.double(res),
             lambda=lambda,y=y,a=a,nsubj=nsubj,
             nu=nu,eps=eps,step=step,y.low=y.low,y.high=y.high,
             y1=y1,alpha=alpha,d=d,nmc=nmc);
  return(llpp$res);
}

