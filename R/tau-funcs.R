.tau1 <- function(y, age, param) {
  y1 <- param$y1
  alpha <- param$alpha
  if (is.na(age)) {
    return(y <= y1)
  }

  tm <- log(y1 / y) / alpha
  return(tm < age & y <= y1)
}

.tau2 <- function(y, age, param) {
  y1 <- param$y1
  alpha <- param$alpha
  r <- param$r
  if (is.na(age)) {
    return(y <= y1)
  }

  tm <- (y1^(1 - r) - y^(1 - r)) / (alpha * (1 - r))
  return(tm < age & y <= y1)
}

.tau30 <- function(y, age, param) {
  y0 <- param$y0
  return(y < y0)
}

.tau31 <- function(y, age, param) {
  y0 <- param$y0
  mu1 <- param$mu1
  y1 <- param$y1
  if (is.na(age)) {
    return(y >= y0 & y < y1)
  }

  tm <- log(y / y0) / mu1
  return(tm < age & y >= y0 & y < y1)
}

.tau32 <- function(y, age, param) {
  y1 <- param$y1
  alpha <- param$alpha
  t1 <- param$t1
  if (is.na(age)) {
    return(y <= y1)
  }

  tm <- t1 + log(y1 / y) / alpha
  return(tm < age & y <= y1)
}

.tau33 <- function(y, age, param) {
  y1 <- param$y1
  t1 <- param$t1
  if (is.na(age)) {
    return(y > y1)
  }

  return(t1 > age & y > y1)
}

.tau40 <- function(y, age, param) {
  y0 <- param$y0
  return(y < y0)
}

.tau41 <- function(y, age, param) {
  y0 <- param$y0
  mu1 <- param$mu1
  y1 <- param$y1
  if (is.na(age)) {
    return(y >= y0 & y < y1)
  }

  tm <- log(y / y0) / mu1
  return(tm < age & y >= y0 & y < y1)
}

.tau42 <- function(y, age, param) {
  y1 <- param$y1
  alpha <- param$alpha
  r <- param$r
  t1 <- param$t1
  if (is.na(age)) {
    return(y <= y1)
  }

  tm <- t1 + (y1^(1 - r) - y^(1 - r)) / (alpha * (1 - r))
  return(tm < age & y <= y1)
}

.tau43 <- function(y, age, param) {
  y1 <- param$y1
  t1 <- param$t1
  if (is.na(age)) {
    return(y > y1)
  }

  return(t1 > age & y > y1)
}

.tau5 <- function(y, age, param) {
  y1 <- param$y1
  alpha <- param$alpha
  yb <- param$yb
  if (is.na(age)) {
    return(y > yb & y <= y1)
  }

  tm <- y1 / (y - yb)
  tm[y <= yb] <- 1
  tm <- log(tm) / alpha
  return(tm < age & y > yb & y <= y1)
}
