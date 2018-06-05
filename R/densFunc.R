.densFunc <- function(yDat, lambda, m, param, fun, par0)
{
  rho <- fun(y = yDat[1], age = yDat[2], lambda = lambda, m = m, param = param, par0 = par0)
  return(mean(rho))
}
