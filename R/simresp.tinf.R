#' @title simulate antibody kinetics of y over a time interval
#' @param t.end end of time interval (beginning is time 0) in days(?)
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
#' @returns This function returns a [list()] with:
#' * t = times (in days, birth at day 0),
#' * b = bacteria level, for each antibody signal
#' (not used; probably meaningless),
#' * y = antibody level, for each antibody signal
#' * smp = whether an infection involves a big jump or a small jump
#' * t.inf = times when infections have occurred.
#' @keywords internal
simresp.tinf <- function(
    lambda,
    t.end,
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

  n.ab <- length(antigen_isos)

  t0 <- 0
  t <- c()
  b <- c()
  ymat <- c()
  t.step <- 1
  smp <- c()
  t.inf <- c()

  # set.seed(975313579)
  t.next <- -log(runif(1, 0, 1)) / lambda # time to first infection...

    mcpar <- ldpar(
      age =
        if (!is.na(age_fixed)) age_fixed else t.next / day2yr,
      antigen_isos,
      nmc,
      predpar = predpar,
      ...)

  par.now <- mcpar

  b.inf <- mcpar[2, ] # b0
  b.end <- rep(0, n.ab)

  if (t.next > t.end) {
    t.next <- t.end - t0
  }

  if (t.next < t.end) {
    t.inf <- c(t.inf, t.next)
  }

  t.now <- seq(from = 0, to = t.next, by = t.step)
  b.now <- array(0, dim = c(length(t.now), n.ab))

  y.now <- array(
    0,
    dim = c(length(t.now), n.ab),
    dimnames = list(
      t = NULL,
      y = antigen_isos
    )
  )

  for (k.ab in 1:n.ab) {
    y.now[, k.ab] <- mk_baseline(k.ab, length(t.now), ...)
  }

  t <- c(t, t0 + t.now)

  b <- rbind(b, b.now)
  ymat <- rbind(ymat, y.now)

  y.end <- as.matrix(y.now)[nrow(y.now), ]

  if (n.ab == 1 && y.end == 0) {
    y.end <- par.now[1]
  }

  if (n.ab > 1) {
    y.end[y.end == 0] <- par.now[1, y.end == 0]
  }

  while (t0 < t.end - t.next) {
    t0 <- t0 + t.next

    if (!renew_params) {
      par.now <- ldpar(
        if (!is.na(age_fixed)) age_fixed else t0 / day2yr,
        antigen_isos,
        nmc,
        predpar = predpar, ...)

      b0 <- b.inf
      # b0 <- runif(n=1,min=1,max=200); not implemented
      par.now[1, ] <- y.end
      # y0 = y at end of prior episode
    }
    # note: the renew_params == TRUE case is handled many lines below

    t.next <- -log(runif(1, 0, 1)) / lambda
    if (t0 <= t.end & t0 + t.next > t.end) {
      t.next <- t.end - t0
    }

    if (t0 + t.next < t.end) {
      t.inf <- c(t.inf, t0 + t.next)
    }

    smp <- rbind(smp, as.vector(symp(par.now)))

    t.now <- seq(from = 0, to = t.next, by = t.step)

    b.now <- ag(t.now, par.now)

    y.now <- ab(t.now, par.now, ...)

    t <- c(t, t0 + t.now)

    b <- rbind(b, b.now)
    ymat <- rbind(ymat, y.now)

    b.end <- b %>% tail(1)
    y.end <- ymat %>% tail(1)

    if (renew_params) {
      if (n_mcmc_samples == 0) {
        nmc <- sample.int(n = mcsize, size = 1)
      }
    }
    # DM: it might be possible to remove these lines and
    # remove the !renew_params condition
    # near the top of the while() loop
      par.now <- ldpar(
        if (!is.na(age_fixed)) age_fixed else (t0 + t.next) / day2yr,
        antigen_isos,
        nmc,
        predpar = predpar,
        ...)
  }
  return(list(
    t = t,
    b = b,
    y = ymat,
    smp = smp,
    t.inf = t.inf
  ))
}
