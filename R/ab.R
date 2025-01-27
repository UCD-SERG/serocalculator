#' kinetics of the antibody (ab) response (power function decay)
#'
#' @param t [numeric] [vector] of elapsed times since start of infection
#' @param par [numeric] [matrix] of model parameters:
#'  - rows are parameters
#'  - columns are biomarkers
#' @inheritDotParams baseline
#'
#' @returns a [matrix()] of predicted biomarker values
#' @examples
#' par1 <- matrix(
#'     c(
#'       1.11418923843475, 1, 0.12415057798022207, 0.24829344792968783,
#'       0.01998946878312856, 0.0012360802436587237, 1.297194045996013,
#'       1.3976510415108334, 1, 0.2159993563893431, 0.4318070551383313,
#'       0.0015146395107173347, 0.0003580062906750277, 1.5695811573082081
#'     ),
#'     nrow = 7L,
#'     ncol = 2L,
#'     dimnames = list(
#'       params = c("y0", "b0", "mu0", "mu1", "c1", "alpha", "shape_r"),
#'       antigen_iso = c("HlyE_IgA", "HlyE_IgG")
#'     )
#'     )
#' t <- 0:1444
#' blims <- matrix(
#'    rep(c(0, 0.5), each = 2L),
#'    nrow = 2L,
#'    ncol = 2L,
#'    dimnames = list(c("HlyE_IgA", "HlyE_IgG"), c("min", "max"))
#'    )
#' preds <- ab(t = t, par = par1, blims = blims)
#'
ab <- function(t, par, ...) {
  t1 <- t1func(par)
  y1 <- y1func(par)
  y0 <- par["y0", ]
  mu1 <- par["mu1", ]
  alpha <- par["alpha", ]
  shape <- par["shape_r", ]
  yt <- array(0, dim = c(length(t), ncol(par)))
  for (k in 1:ncol(par)) {
    u <- (t <= t1[k])
    d <- (t > t1[k])
    yt[u, k] <- y0[k] * exp(mu1[k] * t[u])
    if (shape[k] != 1) {
      # this is a version of Eq 14 from Teunis et al 2014, factoring in the first y1 term
      yt[d, k] <- (y1[k]^(1 - shape[k]) -
        (1 - shape[k]) * alpha[k] * (t[d] - t1[k]))^(1 / (1 - shape[k]))
    }
    if (shape[k] == 1) yt[d, k] <- y1[k] * exp(-alpha[k] * t[d])
    yt[, k] <- baseline(k, yt[, k], ...)
  }
  return(yt)
}
