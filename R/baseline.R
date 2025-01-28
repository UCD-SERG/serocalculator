
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
