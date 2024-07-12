nlm_exit_codes <- c(
  "1" = "1: relative gradient is close to zero, current iterate is probably solution.",
  "2" = "2: successive iterates within tolerance, current iterate is probably solution.",
  "3" = "3: Last global step failed to locate a point lower than x. Either x is an approximate local minimum of the function, the function is too non-linear for this algorithm, or `stepmin` in `est.incidence()` (a.k.a. `steptol` in `nlm()`) is too large.",
  "4" = "4: iteration limit exceeded; increase `iterlim`.",
  "5" = "5: maximum step size `stepmax` exceeded five consecutive times. Either the function is unbounded below, becomes asymptotic to a finite value from above in some direction or `stepmax` is too small."
)
