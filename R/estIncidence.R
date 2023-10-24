
#
#' Age specific seroincidence function
#' add some details here
#'
#' @param dpop cross-sectional population data
#' @param c.age age category
#' @param start starting value for liklihood
#' @param antigen_isos antigen-isotype(s) (a [character()] vector of one or more antigen names)
#' @param noise_params a [data.frame()] containing columns `nu`, etc.
#' @param iterlim a positive integer specifying the maximum number of iterations to be performed before the program is terminated.
#'
#' @return A [data.frame()] containing the following:
#' * `startingval`: the starting guess for incidence rate
#' * `ageCat`: the age category we are analyzing
#' * `incidence.rate`: the estimated incidence rate, per person year
#' * `CI.lwr`: lower limit of confidence interval for incidence rate
#' * `CI.upr`: upper limit of confidence interval for incidence rate
#' * `coverage`: coverage probability
#' * `neg.llik`: negative log-likelihood
#' * `iterations`: the number of iterations used
#'
#' @export
#'
#'
#'
incidence.age <- function(
    dpop,
    c.age,
    antigen_isos,
    noise_params = cond |> filter(Country == "MGH"),
    start=.1,
    iterlim = 100)
  {

  lambda = start # initial estimate: starting value
  log.lambda = log(lambda)
  log.lmin = log(lambda/10)
  log.lmax = log(10*lambda)   # seroincidence rate interval

  c <- deparse(substitute(c))
  cat <- deparse(substitute(cat))

  p_age = dpop %>% filter(ageCat == c.age)
  c_age = dmcmc %>% filter(ageCat == c.age)

  if(ageCat %in% names(cond))
  {
    cond = cond %>% filter(ageCat == c.age)
  }

  ps = list()
  cs = list()
  conds = list()

  for (cur_antigen in antigen_isos)
  {
    ps[cur_antigen] = get_xspd_one_antigen(
      dpop = p_age,
      antigen = cur_antigen)

    cs[cur_antigen] = get_curve_params_one_antigen(
      params = c_age,
      antigen = cur_antigen)

    conds[cur_antigen] =
      noise_params %>%
      filter(antigen_iso == cur_antigen)

  }

  # noise parameters
  # cond.hlye.IgG

  objfunc <- function(llam)
  {

    res = 0

    # add terms, e.g. for other antibodies
    for (cur_antigen in antigen_isos)
    {
      res = res +
        fdev(
          llam,
          ps[[cur_antigen]],
          cs[[cur_antigen]],
          conds[[cur_antigen]])
    }
  }


  # seroincidence estimation
  fit = nlm(
    objfunc,
    log.lambda,
    hessian=TRUE,
    print.level=0,
    iterlim = iterlim,
    stepmax=(log.lmax-log.lmin)/4)

 if(fit$iterations == iterlim)
 {
   warning(
     "Maximum iterations reached; consider increasing `iterlim` argument.")
 }

  log.lambda.est = data.frame(
    startingval = start,
    ageCat = c.age,
    incidence.rate = exp(fit$estimate),
    CI.lwr = exp(fit$estimate + qnorm(c(0.025))*sqrt(1/fit$hessian)),
    CI.upr = exp(fit$estimate + qnorm(c(0.975))*sqrt(1/fit$hessian)),
    coverage = .95,
    neg.llik = fit$minimum,
    iterations = fit$iterations)

  return(log.lambda.est)
}
