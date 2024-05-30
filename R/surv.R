.surv <- function(a, lambda, m) {
  return(pgamma(lambda * a, m + 1, lower.tail = FALSE))
}
