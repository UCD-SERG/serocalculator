
#
#' Age specific seroincidence function
#' add some details here
#'
#' @param dpop cross-sectional population data
#' @param c.age age category
#' @param start starting value for liklihood
#' @param antigen_iso antigen-isotype
#'
#' @return
#' @export
#'
#' @examples
#'
#'
incidence.age <- function(dpop, c.age, antigen_iso, start=.1){

  lambda = start # initial estimate: starting value
  log.lambda = log(lambda)
  log.lmin=log(lambda/10)
  log.lmax=log(10*lambda)   # seroincidence rate interval

  c <- deparse(substitute(c))
  cat <- deparse(substitute(cat))

  #Cross-sectional population data
  p.hlye.IgA <- dpop %>%
    filter(ageCat == c.age) %>%
    filter(.data[["antigen_iso"]] %in% .env[["antigen_iso"]]) %>%
    select(y, a) %>%
    drop_na()

  p.hlye.IgG <- dpop %>%
    filter(antigen_iso == "HlyE_IgG") %>%
    filter(ageCat == c.age) %>%
    select(y, a) %>%
    drop_na()


  c.hlye.IgA <- dmcmc %>% filter(antigen_iso == "HlyE_IgA") %>%
    filter(ageCat == c.age) %>%
    mutate(alpha = alpha*365.25,
           d = r-1) %>%
    select(y1, alpha, d)

  c.hlye.IgG <- dmcmc %>% filter(antigen_iso == "HlyE_IgG") %>%
    filter(ageCat == c.age) %>%
    mutate(alpha = alpha*365.25,
           d = r-1) %>%
    select(y1, alpha, d)


  objfunc <- function(llam){
    # add terms, e.g. for other antibodies
    res <-
      fdev(llam, p.hlye.IgG, c.hlye.IgG, cond.hlye.IgG) +
      fdev(llam, p.hlye.IgA, c.hlye.IgA, cond.hlye.IgA)


  }

  # seroincidence estimation
  fit = nlm(objfunc,log.lambda,
            hessian=TRUE,print.level=0,stepmax=(log.lmax-log.lmin)/4)

  log.lambda.est = c(start,
                     c.age,
                     exp(fit$estimate),
                     exp(fit$estimate + qnorm(c(0.025))*sqrt(1/fit$hessian)),
                     exp(fit$estimate + qnorm(c(0.975))*sqrt(1/fit$hessian)),
                     fit$minimum,
                     fit$iterations)
  return(log.lambda.est)
}
