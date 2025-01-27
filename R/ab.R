#' kinetics of the antibody (ab) response (power function decay)
#'
#' @param t age at infection?
#' @param par parameters
#' @param ... arguments passed to `baseline()`
#'
#' @returns a [matrix()]
#' @export
#' @keywords internal
ab <- function(t, par, ...) {
  t1 <- t1func(par)
  y1 <- y1func(par)
  y0 <- par["y0", ]
  mu1 <- par["mu1", ]
  alpha <- par["alpha", ]
  shape <- par["shape_r", ]
  yt <- array(0, dim = c(length(t), ncol(par)))
  for (k in seq_len(ncol(par))) {
    u <- (t <= t1[k])
    d <- (t > t1[k])
    yt[u, k] <- y0[k] * exp(mu1[k] * t[u])
    if (shape[k] != 1) {
      # this is a version of Eq 14 from Teunis et al 2014,
      # factoring in the first y1 term
      term1 <- y1[k]^(1 - shape[k])
      term2 <- (1 - shape[k]) * alpha[k] * (t[d] - t1[k])
      exponent <- (1 / (1 - shape[k]))
      yt[d, k] <- (term1 - term2)^exponent
    }
    if (shape[k] == 1) yt[d, k] <- y1[k] * exp(-alpha[k] * t[d])
    yt[, k] <- baseline(k, yt[, k], ...)
  }
  return(yt)
}
