.deltaFunc <- function(yDat, lambda, m, param, fun, par0)
{
  deltaRho <-
    fun(y = yDat[2], age = yDat[3], lambda = lambda, m = m, param = param, par0 = par0) -
    fun(y = yDat[1], age = yDat[3], lambda = lambda, m = m, param = param, par0 = par0)

  return(mean(deltaRho))
}
