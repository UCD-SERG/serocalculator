.nllByType <- function(data, param, censorLimit, ivc = FALSE, m = 0, par0, start, modelType)
{
  lambda <- exp(start)
  pdfModel <- switch(modelType,
                     "1" = .rhoPdf1,
                     "2" = .rhoPdf2,
                     "3" = .rhoPdf3,
                     "4" = .rhoPdf4,
                     "5" = .rhoPdf5)
  cdfModel <- switch(modelType,
                     "1" = .rhoCdf1,
                     "2" = .rhoCdf2,
                     "3" = .rhoCdf3,
                     "4" = .rhoCdf4,
                     "5" = .rhoCdf5)

  if (ivc) {
    # data must have .hi and .lo observations
    rho <- apply(X = data, MARGIN = 1, FUN = .deltaFunc,
                 lambda = lambda, m = m, param = param, fun = cdfModel, par0 = par0)
    # Remove empty outcomes
    rho <- rho[rho != 0]
    return(-sum(log(rho)))
  }

  # Deal with uncensored data
  dataUncens <- data[data[, 1] > censorLimit, ]
  if (length(dataUncens) == 2) {
    dataUncens <- as.matrix(t(dataUncens))
  }
  rho <- numeric(0)
  if (length(dataUncens) > 0) {
    rhoUc <- apply(X = dataUncens, MARGIN = 1, FUN = .densFunc,
                   lambda = lambda, m = m, param = param, fun = pdfModel, par0 = par0)
    rho <- c(rho, rhoUc)
  }

  # Deal with censored data
  dataCens <- data[data[, 1] <= censorLimit, ]
  if (length(dataCens) == 2) {
    dataCens[1] <- censorLimit
    dataCens <- as.matrix(t(dataCens))
  }
  if (length(dataCens) > 2) {
    dataCens[, 1] <- censorLimit
  }
  if (length(dataCens) > 0) {
    rhoCn <- apply(X = dataCens, MARGIN = 1, FUN = .densFunc,
                   lambda = lambda, m = m, param = param, fun = cdfModel, par0 = par0)
    rho <- c(rho, rhoCn)
  }

  # Remove empty outcomes
  rho <- rho[!is.na(rho) & rho != 0]
  return(-sum(log(rho)))
}
