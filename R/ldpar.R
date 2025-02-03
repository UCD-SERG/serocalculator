#' @title extract a row from longitudinal parameter set
#' @description
#' take a random sample from longitudinal parameter set
#' given age at infection, for a  list of antibodies
#' @param age age at infection
#' @param antigen_isos antigen isotypes
#' @param nmc mcmc sample to use
#' @param ... passed to `simpar()`
#' @param npar number of parameters
#' @returns an array of parameters:
#'  c(y0,b0,mu0,mu1,c1,alpha,shape)
#' @keywords internal
ldpar <- function(age, antigen_isos, nmc, npar, ...) {
  dimnames1 <- list(
    params = c("y0", "b0", "mu0", "mu1", "c1", "alpha", "shape_r"),
    antigen_iso = antigen_isos
  )
  spar <- array(
    NA,
    dim = c(
      2 + npar, # 2 additional parameters
      length(antigen_isos)
    ),
    dimnames = dimnames1
  )

  for (k.test in antigen_isos) {
    spar[, k.test] <- simpar(age, k.test, nmc, ...)
  }
  return(spar)
}
