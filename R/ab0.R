# uses r > 1 scale for shape
ab0 <- function(t, curve_params) {
  y0 <- curve_params[["y0"]]
  y1 <- curve_params[["y1"]]
  t1 <- curve_params[["t1"]]
  alpha <- curve_params[["alpha"]]
  r <- curve_params[["r"]]

  beta <- bt(y0, y1, t1)

  yt <- 0

  yt_phase_1 <- y0 * exp(beta * t)
  if (r == 1) {
    yt_phase_2 <- y1 * exp(-alpha * (t - t1))
    # see wolfram alpha result: https://bit.ly/3ZB69Yn
    # this is a version the product-limit characterization of the
    # exponential function
  } else {
    yt_phase_2 <-
      (y1^(1 - r) - (1 - r) * alpha * (t - t1))^(1 / (1 - r))
  }

  yt <- dplyr::if_else(t <= t1, yt_phase_1, yt_phase_2)
  return(yt)
}
