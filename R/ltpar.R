# not used?
ltpar <- function(k.test, ...) {
  y0.mc       <- par.pred(1, k.test, ...)

  y1.mc       <- par.pred(2, k.test, ...)

  t1.mc       <- par.pred(3, k.test, ...)

  alpha.mc    <- par.pred(4,k.test, ...);
  shape.mc    <- par.pred(5,k.test, ...);
  return(
    data.frame(
      "y1"=y1.mc,
      "alpha"=alpha.mc,
      "r"=shape.mc,
      "y0"=y0.mc,
      "t1"=t1.mc));
}
