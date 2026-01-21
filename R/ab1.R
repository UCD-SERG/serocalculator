# uses r > 1 scale for shape
ab1 <- function(t, y0, y1, t1, alpha, shape) {
  beta <- bt(y0, y1, t1)
  yt <- ifelse(
    t <= t1,
    y0 * exp(beta * t),
    (y1^(1 - shape) - (1 - shape) * alpha * (t - t1))^(1 / (1 - shape))
  )
  return(yt)
}
