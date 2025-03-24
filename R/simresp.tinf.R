#' @title simulate antibody kinetics of y over a time interval
#' @param t_end end of time interval (beginning is time 0) in days(?)
#' @param predpar an [array()] with dimensions named:
#' * `antigen_iso`
#' * `parameter`
#' * `obs`
#' @param lambda seroconversion rate (1/days),
#' @param age_fixed
#' parameter estimates for fixed age (age_fixed in years) or not.
#' when age_fixed = NA then age at infection is used.
#' @param antigen_isos antigen isotypes
#' @param n_mcmc_samples a posterior sample may be selected (1:4000), or not
#' when n_mcmc_samples = 0 a posterior sample is chosen at random.
#' @param renew_params At infection,
#' a new parameter sample may be generated
#' (when `renew_params = TRUE`).
#' Otherwise (when `renew_params = FALSE`),
#' a sample is generated at birth and kept,
#' but baseline y0 are carried over from prior infections.
#' @param ... additional arguments passed to
#' [ldpar()], [mk_baseline()], and [ab()]
#' @inheritDotParams ldpar
#' @inheritDotParams ab
#' @inheritDotParams mk_baseline
#' @returns a [list] with:
#' * t = times (in days, birth at day 0),
#' * b = bacteria level, for each antibody signal
#' (not used; probably meaningless),
#' * y = antibody level, for each antibody signal
#' * smp = whether an infection involves a big jump or a small jump
#' * t.inf = times when infections have occurred.
#' @keywords internal
simresp.tinf <- function(# nolint: object_name_linter
    lambda,
    t_end,
    age_fixed,
    antigen_isos,
    n_mcmc_samples = 0,
    renew_params,
    predpar,
    ...) {
  mcsize <- dim(predpar)[3]
  nmc <- n_mcmc_samples

  day2yr <- 365.25

  if (n_mcmc_samples == 0) {
    nmc <- sample.int(n = mcsize, size = 1)
  }

  n_ab <- length(antigen_isos)

  t0 <- 0
  t <- c()
  b <- c()
  y_mat <- c()
  t_step <- 1
  smp <- c()
  t_inf <- c()

  t_next <- -log(runif(1, 0, 1)) / lambda # time to first infection...

  age <- if_else(!is.na(age_fixed), age_fixed, t_next / day2yr)

  mcpar <- ldpar(
    age = age,
    antigen_isos,
    nmc,
    predpar = predpar,
    ...
  )

  par_now <- mcpar

  if (t_next > t_end) {
    t_next <- t_end - t0
  }

  if (t_next < t_end) {
    t_inf <- c(t_inf, t_next)
  }

  t_now <- seq(from = 0, to = t_next, by = t_step)
  b_now <- array(0, dim = c(length(t_now), n_ab))

  y_now <- array(
    0,
    dim = c(length(t_now), n_ab),
    dimnames = list(
      t = NULL,
      y = antigen_isos
    )
  )

  for (cur_ab in 1:n_ab) {
    y_now[, cur_ab] <- mk_baseline(cur_ab, length(t_now), ...)
  }

  t <- c(t, t0 + t_now)

  b <- rbind(b, b_now)
  y_mat <- rbind(y_mat, y_now)

  y_end <- as.matrix(y_now)[nrow(y_now), ]

  if (n_ab == 1 && y_end == 0) {
    y_end <- par_now[1]
  }

  if (n_ab > 1) {
    y_end[y_end == 0] <- par_now[1, y_end == 0]
  }

  while (t0 < t_end - t_next) {
    t0 <- t0 + t_next

    if (!renew_params) {
      par_now <- ldpar(
        if (!is.na(age_fixed)) age_fixed else t0 / day2yr,
        antigen_isos,
        nmc,
        predpar = predpar, ...
      )

      # b0 <- runif(n=1,min=1,max=200); not implemented
      par_now[1, ] <- y_end
      # y0 = y at end of prior episode
    }
    # note: the renew_params == TRUE case is handled many lines below

    t_next <- -log(runif(1, 0, 1)) / lambda
    if (t0 <= t_end && t0 + t_next > t_end) {
      t_next <- t_end - t0
    }

    if (t0 + t_next < t_end) {
      t_inf <- c(t_inf, t0 + t_next)
    }

    smp <- rbind(smp, as.vector(symp(par_now)))

    t_now <- seq(from = 0, to = t_next, by = t_step)

    b_now <- ag(t_now, par_now)

    y_now <- ab(t_now, par_now, ...)

    t <- c(t, t0 + t_now)

    b <- rbind(b, b_now)
    y_mat <- rbind(y_mat, y_now)

    y_end <- y_mat %>% tail(1)

    if (renew_params) {
      if (n_mcmc_samples == 0) {
        nmc <- sample.int(n = mcsize, size = 1)
      }
    }
    # DM: it might be possible to remove these lines and
    # remove the !renew_params condition
    # near the top of the while() loop
    age <- if_else(
      !is.na(age_fixed),
      age_fixed,
      (t0 + t_next) / day2yr
    )

    par_now <- ldpar(
      age = age,
      antigen_isos,
      nmc,
      predpar = predpar,
      ...
    )
  }
  return(list(
    t = t,
    b = b,
    y = y_mat,
    smp = smp,
    t.inf = t_inf
  ))
}
