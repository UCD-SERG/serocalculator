
# calculate a few additional variables needed for the simulation
# parameter vector: c(y0,b0,mu0,mu1,c1,alpha,shape)
afunc <- function(par) (par[4, ] - par[3, ]) / (par[5, ] * par[1, ])
bfunc <- function(par) par[4, ] / (par[4, ] - par[3, ])
t1func <- function(par) log(1 + afunc(par) * par[2, ]) / (par[4, ] - par[3, ])
y1func <- function(par) par[1, ] * (1 + afunc(par) * par[2, ])^(bfunc(par))

# symptomatic (1) or asymptomatic (0) seroconversion?
symp <- function(par) {
  y0 <- par["y0", ]
  b0 <- par["b0", ]
  mu0 <- par["mu0", ]
  c1 <- par["c1", ]
  ymin <- mu0 * b0 / c1
  return(as.numeric(y0 <= ymin))
}

# the function f() linking pre- and post- antibody levels
transf <- function(y0, par) {
  b0 <- par["b0", ]
  mu0 <- par["mu0", ]
  mu1 <- par["mu1", ]
  c1 <- par["c1", ]
  cc1 <- mu1 / (mu1 - mu0)
  cc2 <- (mu1 - mu0) * b0 / c1
  return(y0 * (1 + cc2 / y0)^cc1)
}

#' @title
#' Substitute baseline values
#' @description
#' whenever y is below a cutoff (`blims[kab,2]`), substitute a random sample
#' from a baseline distribution
#'
#' @param yvec a [numeric] [vector] of predicted biomarker values,
#' for one biomarker
#' @inheritParams mk_baseline
#' @param ... unused
#'
#' @returns an altered version of `yvec`
#' @dev
#'
baseline <- function(kab, yvec, blims, ...) {
  subst <- which(yvec < blims[kab, 2])
  k <- 1
  while (k <= length(subst)) {
    yvec[subst[k]] <- mk_baseline(kab, 1, blims)
    k <- k + 1
  }
  return(yvec)
}

#' @title generate random sample from baseline distribution
#' @param kab [integer] indicating which row to read from `blims`
#' @param n number of observations
#' @param blims range of possible baseline antibody levels
#' @param ... not currently used
#' @return a [numeric()] vector
#' @dev
mk_baseline <- function(kab, n = 1, blims, ...) {
  # yset <- rlnorm(n=1,meanlog=negpar[1],sdlog=negpar[2]);
  if (blims[kab, 2] == 0) {
    yset <- rep(0, n)
  } else {
    yset <- runif(
      n = n,
      min = blims[kab, "min"],
      max = blims[kab, "max"]
    )
  }
  return(yset)
}
