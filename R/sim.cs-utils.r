#' @title extract a row from longitudinal parameter set
#' @description
#'  take a random sample from longitudinal parameter set
#' given age at infection, for a  list of antibodies
#'
#' @param age age at infection
#' @param antigen_isos antigen isotypes
#' @param nmc mcmc sample to use
#' @param ... passed to `simpar()`
#' @param npar number of parameters
#'
#' @returns an array of parameters:
#'  c(y0,b0,mu0,mu1,c1,alpha,shape)
ldpar <- function(
    age,antigen_isos,nmc,npar,...)
{
  dimnames1 = list(
    params = c("y0","b0","mu0","mu1","c1","alpha","shape_r"),
    antigen_iso = antigen_isos
  )
  spar <- array(
    NA,
    dim = c(
      2+npar, # 2 additional parameters
      length(antigen_isos)),
    dimnames = dimnames1);

  for(k.test in antigen_isos)
  {
    spar[,k.test] <- simpar(age, k.test, nmc, ...);
  }
  return(spar);
}

# calculate a few additional variables needed for the simulation
# parameter vector: c(y0,b0,mu0,mu1,c1,alpha,shape)
afunc  <- function(par) (par[4,]-par[3,])/(par[5,]*par[1,])
bfunc  <- function(par) par[4,]/(par[4,]-par[3,])
t1func <- function(par) log(1+afunc(par)*par[2,])/(par[4,]-par[3,])
y1func <- function(par) par[1,]*(1+afunc(par)*par[2,])^(bfunc(par))



#
#' kinetics of the antibody (ab) response (power function decay)
#'
#' @param t age at infection?
#' @param par parameters
#' @param ... arguments passed to `baseline()`
#'
#' @returns a [matrix()]
ab <- function(t,par,...) {
  t1 <- t1func(par);
  y1 <- y1func(par);
  y0 <- par["y0",];
  mu1 <- par["mu1",];
  alpha <- par["alpha",];
  shape <- par["shape_r",];
  yt <- array(0,dim=c(length(t),ncol(par)));
  for(k in 1:ncol(par)){
    u <- (t <= t1[k]); d <- (t > t1[k]);
    yt[u,k] <- y0[k] * exp(mu1[k] * t[u]);
    if(shape[k]!=1)
    {
      # this is a version of Eq 14 from Teunis et al 2014, factoring in the first y1 term
      yt[d,k] <- (y1[k]^(1 - shape[k]) -
                    (1-shape[k]) * alpha[k] * (t[d] - t1[k]))^(1/(1 - shape[k]));
    }
    if(shape[k]==1) yt[d,k] <- y1[k] * exp(-alpha[k] * t[d]);
    yt[,k] <- baseline(k,yt[,k], ...);
  }
  return(yt);
}

# symptomatic (1) or asymptomatic (0) seroconversion?
symp <- function(par){
  y0 <- par["y0",];
  b0 <- par["b0",];
  mu0 <- par["mu0",];
  c1 <- par["c1",];
  ymin <- mu0*b0/c1;
  return(as.numeric(y0 <= ymin));
}

# the function f() linking pre- and post- antibody levels
transf <- function(y0,par){
  b0 <- par["b0",];
  mu0 <- par["mu0",];
  mu1 <- par["mu1",];
  c1 <- par["c1",];
  cc1 <- mu1/(mu1-mu0);
  cc2 <- (mu1-mu0)*b0/c1;
  return(y0*(1+cc2/y0)^cc1);
}

# whenever y is below a cutoff (blims[kab,2]) substitute a random sample
# from a baseline distribution
baseline <- function(kab,yvec, blims, ...){
  subst <- which(yvec < blims[kab,2]); k <- 1;
  while(k <= length(subst)){
    yvec[subst[k]] <- mkbaseline(kab,1, blims);
    k <- k + 1;
  }
  return(yvec);
}

#
#' @title generate random sample from baseline distribution
#'
#' @param kab index for which row of antibody baseline limits to read from `blims`
#' @param n number of observations
#' @param blims range of possible baseline antibody levels
#' @param ... not currently used
#'
#' @return a [numeric()] vector
#' @export
#'
mkbaseline <- function(kab,n=1, blims, ...){
  # yset <- rlnorm(n=1,meanlog=negpar[1],sdlog=negpar[2]);
  if(blims[kab,2] == 0)
  {
    yset <- rep(0,n);
  } else
  {
    yset <- runif(
      n = n,
      min = blims[kab, "min"],
      max = blims[kab, "max"])

  }
  return(yset);
}
