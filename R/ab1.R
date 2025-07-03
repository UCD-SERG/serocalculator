# uses r > 1 scale for shape
ab1 <- function(t, y0, y1, t1, alpha, shape) {
  beta <- bt(y0, y1, t1)

  yt <- 0

  if (t <= t1) {
    yt <- y0 * exp(beta * t)
  }

  if (t > t1) {
    yt <- (y1^(1 - shape) - (1 - shape) * alpha * (t - t1))^(1 / (1 - shape))
  }

  return(yt)
}
