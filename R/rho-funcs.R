.rhoCdf <- function(y, par)
{
  return(plnorm(y, meanlog = par[1], sdlog = par[2]))
}

.rhoCdf1 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  nMc <- length(y1)
  rho <- rep(0, nMc)
  if (y <= 0) {
    return(rho)
  }
  index <- .tau1(y, age, param)
  for (j in 0:m) {
    rho[index] <- rho[index] +
      pgamma(q = log(y1[index] / y) * lambda / alpha[index],
             shape = j + 1,
             rate = 1,
             lower.tail = FALSE) /
      factorial(j)
  }
  rho[index] <- rho[index] / (m + 1)
  rho[y > y1] <- 1
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoCdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoCdf2 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  r <- param$r
  nMc <- length(y1)
  rho <- rep(0, nMc)
  if (y <= 0) {
    return(rho)
  }

  index <- .tau2(y, age, param)
  for (j in 0:m) {
    rho[index] <- rho[index] +
      pgamma(q = lambda * (y1[index]^(1 - r[index]) - y^(1 - r[index])) /
               (alpha[index] * (1 - r[index])),
             shape = j + 1,
             rate = 1,
             lower.tail = FALSE) /
      factorial(j)
  }
  rho[index] <- rho[index] / (m + 1)
  rho[y > y1] <- 1
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoCdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoCdf3 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  y0 <- param$y0
  mu1 <- param$mu1
  nMc <- length(y1)
  # Rising branch
  rho1 <- rep(0, nMc)
  # Decaying branch
  rho2 <- rep(0, nMc)
  if (y <= 0) {
    return(rho1)
  }

  # Then y = y0 in rho1
  index0 <- .tau30(y, age, param)
  # Then y = y in rho1
  index1 <- .tau31(y, age, param)
  # Then y = y in rho2
  index2 <- .tau32(y, age, param)
  # Then y = y1 in rho1 and in rho2
  index3 <- .tau33(y, age, param)
  for (j in 0:m) {
    rho1[index0] <- rho1[index0] + 1 / factorial(j)
    rho1[index1] <- rho1[index1] +
      pgamma(q = lambda * log(y / y0[index1]) / mu1[index1],
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
    rho1[index3] <- rho1[index3] +
      pgamma(q = lambda * log(y1[index3] / y0[index3]) / mu1[index3],
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
    rho2[index2] <- rho2[index2] +
      pgamma(q = (log(y1[index2] / y0[index2]) / mu1[index2] +
                    log(y1[index2] / y) / alpha[index2]) * lambda,
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
    rho2[index3] <- rho2[index3] +
      pgamma(q = (log(y1[index3] / y0[index3]) / mu1[index3]) * lambda,
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
  }
  rho1 <- 1 - rho1 / (m + 1)
  rho2 <- rho2 / (m + 1)
  rho <- rho1 + rho2
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoCdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoCdf4 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  r <- param$r
  y0 <- param$y0
  mu1 <- param$mu1
  nMc <- length(y1)
  # Rising branch
  rho1 <- rep(0, nMc)
  # Decaying branch
  rho2 <- rep(0, nMc)
  if (y <= 0) {
    return(rho1)
  }

  # Then y = y0 in rho1
  index0 <- .tau40(y, age, param)
  # Then y = y in rho1
  index1 <- .tau41(y, age, param)
  # Then y = y in rho2
  index2 <- .tau42(y, age, param)
  # Then y = y1 in rho1 and in rho2
  index3 <- .tau43(y, age, param)
  for (j in 0:m) {
    rho1[index0] <- rho1[index0] + 1 / factorial(j)
    rho1[index1] <- rho1[index1] +
      pgamma(q = lambda * log(y / y0[index1]) / mu1[index1],
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
    rho1[index3] <- rho1[index3] +
      pgamma(q = lambda * log(y1[index3] / y0[index3]) / mu1[index3],
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
    rho2[index2] <- rho2[index2] +
      pgamma(q = lambda * (log(y1[index2] / y0[index2]) / mu1[index2] +
                             (y1[index2]^(1 - r[index2]) - y^(1 - r[index2])) /
                             (alpha[index2] * (1 - r[index2]))),
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
    rho2[index3] <- rho2[index3] +
      pgamma(q = lambda * (log(y1[index3] / y0[index3]) / mu1[index3]),
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
  }
  rho1 <- 1 - rho1 / (m + 1)
  rho2 <- rho2 / (m + 1)
  rho <- rho1 + rho2
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoCdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoCdf5 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  yb <- param$yb
  nMc <- length(y1)
  rho <- rep(0, nMc)
  if (y <= 0) {
    return(rho)
  }

  index <- .tau5(y, age, param)
  for (j in 0:m) {
    rho[index] <- rho[index] +
      pgamma(q = log(y1[index] / (y - yb[index])) * lambda / alpha[index],
             shape = j + 1, rate = 1, lower.tail = FALSE) /
      factorial(j)
  }
  rho[index] <- rho[index] / (m + 1)
  rho[y > y1] <- 1
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoCdf(y, par0)
  }
  return(rho[rho != 0])
}


.rhoPdf <- function(y, par) {
  return(dlnorm(y, meanlog = par[1], sdlog = par[2]))
}

.rhoPdf1 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  nMc <- length(y1)
  rho <- rep(0, nMc)
  if (y <= 0) {
    return(rho)
  }

  index <- .tau1(y, age, param)
  deltaTP <- (m + 1) / lambda
  rho[index] <-
    pgamma(q = log(y1[index] / y) / alpha[index],
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (alpha[index] * deltaTP * y)
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoPdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoPdf2 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  r <- param$r
  nMc <- length(y1)
  rho <- rep(0, nMc)
  if (y <= 0) {
    return(rho)
  }

  index <- .tau2(y, age, param)
  deltaTP <- (m + 1) / lambda
  rho[index] <-
    pgamma(q = (y1[index]^(1 - r[index]) - y^(1 - r[index])) / (alpha[index] * (1 - r[index])),
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (alpha[index] * deltaTP * y^(r[index]))
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoPdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoPdf3 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  y0 <- param$y0
  mu1 <- param$mu1
  nMc <- length(y1)
  # Rising branch
  rho1 <- rep(0, nMc)
  # Decaying branch
  rho2 <- rep(0, nMc)
  if (y <= 0) {
    return(rho1)
  }
  index1 <- .tau31(y, age, param)
  index2 <- .tau32(y, age, param)
  deltaTP <- (m + 1) / lambda
  rho1[index1] <-
    pgamma(q = log(y / y0[index1]) / mu1[index1],
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (mu1[index1] * deltaTP * y)
  rho2[index2] <-
    pgamma(q = log(y1[index2] / y0[index2]) / mu1[index2] + log(y1[index2] / y) / alpha[index2],
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (alpha[index2] * deltaTP * y)
  rho <- rho1 + rho2
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoPdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoPdf4 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  r <- param$r
  y0 <- param$y0
  mu1 <- param$mu1
  nMc <- length(y1)
  # Rising branch
  rho1 <- rep(0, nMc)
  # Decaying branch
  rho2 <- rep(0, nMc)
  if (y < 0) {
    return(rho1)
  }
  index1 <- .tau41(y, age, param)
  index2 <- .tau42(y, age, param)
  deltaTP <- (m + 1) / lambda
  rho1[index1] <-
    pgamma(q = log(y / y0[index1]) / mu1[index1],
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (mu1[index1] * deltaTP * y)
  rho2[index2] <-
    pgamma(q = log(y1[index2] / y0[index2]) / mu1[index2] +
             (y1[index2]^(1 - r[index2]) - y^(1 - r[index2])) /
             (alpha[index2] * (1 - r[index2])),
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (alpha[index2] * deltaTP * y^(r[index2]))
  rho <- rho1 + rho2
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoPdf(y, par0)
  }
  return(rho[rho != 0])
}

.rhoPdf5 <- function(y, age, lambda, m, param, par0)
{
  y1 <- param$y1
  alpha <- param$alpha
  yb <- param$yb
  nMc <- length(y1)
  rho <- rep(0, nMc)
  if (y <= 0) {
    return(rho)
  }
  index <- .tau5(y, age, param)
  deltaTP <- (m + 1) / lambda
  rho[index] <-
    pgamma(q = log(y1[index] / (y - yb[index])) / alpha[index],
           shape = m + 1, rate = lambda, lower.tail = FALSE) /
    (alpha[index] * deltaTP * (y - yb[index]))
  if (!is.na(age)) {
    sProb <- .surv(age, lambda, m)
    rho <- rho + sProb * .rhoPdf(y, par0)
  }
  return(rho[rho != 0])
}
