nlm_exit_codes = c(
  "1" = "1: relative gradient is close to zero, current iterate is probably solution.",
  "2" = "2: successive iterates within tolerance, current iterate is probably solution.",
  "3" = "3: last global step failed to locate a point lower than estimate. Either estimate is an approximate local minimum of the function or steptol is too small.",
  "4" = "4: iteration limit exceeded.",
  "5" = "5: maximum step size stepmax exceeded five consecutive times. Either the function is unbounded below, becomes asymptotic to a finite value from above in some direction or stepmax is too small."
)

usethis::use_data(nlm_exit_codes, overwrite = TRUE, internal = TRUE)
