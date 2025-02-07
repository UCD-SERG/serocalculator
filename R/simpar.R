# age doesn't seem to be used?
# this function converts the
# 4 parameter + y0 format ("5 parameters + initial conditions)
# into the 5 parameters + y0 + y1 + t1 format ("3 parameters + initial conditions")
# it assumes that b0 = 1, and seems to derive one
# extra degree of freedom from the linked differential equations
simpar <- function(age, k.test, nmc, ...) {
  y0.mc <- par.pred.n(1, k.test, nmc, ...)
  b0.mc <- 1
  y1.mc <- par.pred.n(2, k.test, nmc, ...)
  t1.mc <- par.pred.n(3, k.test, nmc, ...)
  alpha.mc <- par.pred.n(4, k.test, nmc, ...)
  shape.mc <- par.pred.n(5, k.test, nmc, ...)
  sp <- estpart1(y0.mc, y1.mc, t1.mc)
  mu1.mc <- mu1fn(y0.mc, y1.mc, t1.mc)
  mu0.mc <- mu0fn(sp[1], y0.mc, y1.mc, t1.mc)
  c1.mc <- sp[2]
  to_return <- c(
    y0 = y0.mc,
    b0 = b0.mc,
    mu0 = mu0.mc,
    mu1 = mu1.mc,
    c1 = c1.mc,
    alpha = alpha.mc,
    r = shape.mc
  )
  return(to_return)
}
