
#' collect cross-sectional data
#'
#' @description output: (age, y(t) set)
#'
#' @param lambda seroconversion rate (in events/person-day)
#' @param n.smpl number of samples n.smpl (= nr of simulated records)
#' @param age.rng age range to use for simulating data, in days
#' @param age.fx age.fx for parameter sample (age.fx = NA for age at infection)
#' @param antigen_isos Character vector with one or more antibody names. Values must match `curve_params`.
#' @param n.mc
#' * when `n.mc` is in 1:4000 a fixed posterior sample is used
#' * when n.mc = 0 a random smaple is chosen
#' @param renew.params
#' * `renew.params = TRUE` generates a new parameter set for each infection
#' * `renew.params = FALSE` keeps the one selected at birth, but updates baseline y0
#' @param ... arguments passed to [simresp.tinf()]
#'
#' @return an [array()]
#'
simcs.tinf <- function(
    lambda,
    n.smpl,
    age.rng,
    age.fx = NA,
    antigen_isos,
    n.mc = 0,
    renew.params = FALSE,
    ...)
{
  st.days <- round(age.rng[1])
  # from min=age.rng[1] days...
  en.days <- round(age.rng[2])
  # to   max=age.rng[2] days...
  if (st.days == 0)
    st.days <- 1

  # if(en.days>30000) en.days <- 30000;
  y.smpl <- array(
    NA,
    dim = c(n.smpl, length(antigen_isos) + 1),
    dimnames = list(
      obs = 1:n.smpl,
      var = c("age", antigen_isos)))
  # y and age
  for (k.smpl in 1:n.smpl)
  {
    resp <-
      simresp.tinf(
        lambda,
        t.end = en.days,
        age.fx = age.fx,
        antigen_isos = antigen_isos,
        n.mc = n.mc,
        renew.params = renew.params,
        ...
      )

    tinf.smp <-
      sample((st.days:en.days), size = 1)
    # sample at random age
    y.smpl[k.smpl, ] <-
      c(resp$t[tinf.smp], as.matrix(resp$y)[tinf.smp, ])

  }
  return(y.smpl)

}
