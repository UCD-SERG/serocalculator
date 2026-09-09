# Compare the two candidate time-since-infection densities.
#
# The methodology article's `#prp-time-since-infection` and the density
# that `src/serocalc.c` evaluates disagree about how the probability of
# having seroconverted is spread over [0, age]. This script measures the
# difference two ways: against a directly simulated Poisson process, and
# as the resulting bias in the fitted incidence rate.
#
# Backs the measurements reported in
# <https://github.com/UCD-SERG/serocalculator/issues/687>.
#
# Both densities below are conditional on having seroconverted at least
# once. The never-infected branch has mass exp(-lambda * age) under both
# forms, so it carries no information about which one is right; the whole
# difference is in the shape over [0, age].


# ---- where the two forms come from -----------------------------------
#
# `dens_from_birth` is exact for a homogeneous Poisson process of rate
# lambda running on [0, age]. Writing T for the time since the most
# recent event, T exceeds t exactly when no event falls in the last t
# and at least one falls before that:
#
#   P(T > t) = exp(-lambda * t) * (1 - exp(-lambda * (age - t)))
#            = exp(-lambda * t) - exp(-lambda * age)
#
# Differentiating gives lambda * exp(-lambda * t) on [0, age], with the
# remaining exp(-lambda * age) on "no event ever occurred". Dividing by
# 1 - exp(-lambda * age) conditions on having seroconverted, which is the
# form implemented above.
#
# `dens_teunis_2020` comes from a different construction. Appendix A of
# that paper assumes the subject sits at a uniformly random point of the
# inter-infection interval containing them, truncated at their age:
#
#   u_f(tau | dt, age) = [0 <= tau <= min(dt, age)] / min(dt, age)
#
# and integrates that over a length-biased interval distribution. The
# `min(dt, age)` is what produces the extra term: when the interval is
# longer than the subject's life, the most recent infection is placed
# uniformly during that life rather than derived. Equivalently, the
# infinite-horizon density lambda * exp(-lambda * tau) has its tail
# beyond `age` folded back uniformly over [0, age]; that tail has mass
# exp(-lambda * age), which is the mass of the uniform term.
#
# The two forms are not distinguishable by any normalization check.
# Both integrate to 1 - exp(-lambda * age) over [0, age] once scaled into
# the full mixture, and both leave exp(-lambda * age) for the
# never-infected branch. Only the shape over [0, age] differs, the 2020
# form being flatter.
#
# What the measurements below establish: `dens_from_birth` reproduces a
# directly simulated Poisson process to three decimals and
# `dens_teunis_2020` does not, so for the constant-rate model the package
# states, the article's form is exact and the engine's is an
# approximation. The cost of the choice is small even so, because the
# shared never-infected atom carries most of the information about
# lambda: fitting one model to the other's data moves the estimate by
# about 1.6 to 3.3 percent over the SEES age distribution, peaking near
# lambda * age = 1 to 2.
#
# Neither is therefore "the wrong formula". The 2020 construction
# generalizes to non-exponential inter-infection intervals, which the
# from-birth derivation does not; it pays for that with a stationarity
# assumption a finite-lived host violates.


# ---- the two candidate densities ------------------------------------

# Exact for a homogeneous Poisson process started at birth, which is the
# model `@def-constant-incidence-model` states.
dens_from_birth <- function(time, lambda, age) {
  lambda * exp(-lambda * time) / (1 - exp(-lambda * age))
}

# Teunis and van Eijkeren (2020), Statistics in Medicine 39(21):2799-2814,
# Equation (4). This is what `dnsF()` and `prbF()` evaluate, up to the
# factor of `Pa` that scales it into the full mixture.
dens_teunis_2020 <- function(time, lambda, age) {
  lambda * exp(-lambda * time) + exp(-lambda * age) / age
}


# ---- generating processes -------------------------------------------

# Draw the time since the most recent event of a Poisson process running
# on [0, age], or NA when no event occurred. Assumes neither density
# above: it places the events and then measures back from `age`.
sim_from_birth <- function(lambda, age) {
  n_events <- stats::rpois(1, lambda * age)
  if (n_events == 0) {
    return(NA_real_)
  }
  age - max(stats::runif(n_events, 0, age))
}

# Draw from the 2020 model's own mixture: never infected with probability
# exp(-lambda * age); otherwise uniform on [0, age] with that same
# probability, and truncated exponential the rest of the time.
sim_teunis_2020 <- function(lambda, age) {
  never_infected <- exp(-lambda * age)
  if (stats::runif(1) < never_infected) {
    return(NA_real_)
  }
  if (stats::runif(1) < never_infected) {
    return(stats::runif(1, 0, age))
  }
  -log(1 - stats::runif(1) * (1 - never_infected)) / lambda
}


# ---- estimation ------------------------------------------------------

# Negative log-likelihood for directly observed times since infection.
# `times` is NA for subjects with no seroconversion; `ages` is the same
# length. `model` selects which density the infected branch uses.
nll_time_since_infection <- function(lambda, times, ages, model) {
  model <- match.arg(model, c("from_birth", "teunis_2020"))
  if (lambda <= 0) {
    return(Inf)
  }
  infected <- !is.na(times)
  never_infected_term <- sum(-lambda * ages[!infected])
  density <- switch(model,
    from_birth = dens_from_birth(times[infected], lambda, ages[infected]),
    teunis_2020 = dens_teunis_2020(times[infected], lambda, ages[infected])
  )
  # Both branches are stated conditional on infection, so scale by the
  # probability of being in that branch to get the full mixture.
  infected_term <- sum(
    log(1 - exp(-lambda * ages[infected])) + log(density)
  )
  -(never_infected_term + infected_term)
}

fit_lambda <- function(times, ages, model, interval = c(1e-4, 3)) {
  stats::optimize(
    nll_time_since_infection,
    interval = interval,
    times = times,
    ages = ages,
    model = model
  )$minimum
}


# ---- measurement 1: which density matches the Poisson process --------

# Simulate from the process itself and compare the empirical conditional
# density against both candidates. Returns one row per histogram bin.
compare_densities <- function(lambda = 0.1,
                              age = 10,
                              n_draws = 4e5,
                              bin_width = 1,
                              seed = 1) {
  set.seed(seed)
  times <- vapply(
    seq_len(n_draws),
    function(i) sim_from_birth(lambda, age),
    numeric(1)
  )
  infected <- !is.na(times)
  breaks <- seq(0, age, by = bin_width)
  binned <- graphics::hist(times[infected], breaks = breaks, plot = FALSE)
  data.frame(
    time = binned$mids,
    simulated = binned$density,
    from_birth = dens_from_birth(binned$mids, lambda, age),
    teunis_2020 = dens_teunis_2020(binned$mids, lambda, age)
  )
}


# ---- measurement 2: what the choice costs in lambda-hat --------------

# Fit both models to data generated by each, over a supplied age
# distribution. Each model should recover its own truth; the off-diagonal
# rows are the misspecification bias.
compare_fits <- function(ages,
                         lambdas = c(0.05, 0.1, 0.2, 0.5),
                         n_replicates = 40,
                         seed = 2) {
  set.seed(seed)
  simulators <- list(
    from_birth = sim_from_birth,
    teunis_2020 = sim_teunis_2020
  )
  replicated_ages <- rep(ages, n_replicates)
  rows <- lapply(lambdas, function(lambda) {
    lapply(names(simulators), function(generator) {
      simulate_one <- simulators[[generator]]
      times <- vapply(
        replicated_ages,
        function(age) simulate_one(lambda, age),
        numeric(1)
      )
      data.frame(
        true_lambda = lambda,
        generated_by = generator,
        fit_from_birth = fit_lambda(times, replicated_ages, "from_birth"),
        fit_teunis_2020 = fit_lambda(times, replicated_ages, "teunis_2020")
      )
    })
  })
  fits <- do.call(rbind, unlist(rows, recursive = FALSE))
  fits$ratio <- fits$fit_teunis_2020 / fits$fit_from_birth
  fits
}


# ---- share of the density carried by the age-correction term ---------

# The term the article omits integrates to exp(-lambda * age) over
# [0, age], so that is its share of the infected-branch density.
correction_share <- function(ages, lambdas = c(0.05, 0.1, 0.2)) {
  data.frame(
    lambda = lambdas,
    mean_share = vapply(
      lambdas,
      function(lambda) mean(exp(-lambda * ages)),
      numeric(1)
    ),
    share_at_median_age = exp(-lambdas * stats::median(ages))
  )
}


# ---- reproducing the figures in issue #687 ---------------------------
#
# subject_ages <- unique(sees_pop_data_100[, c("id", "age")])
# ages <- subject_ages$age
# ages <- ages[!is.na(ages) & ages > 0]
#
# compare_densities()          # measurement 1, table of densities
# compare_fits(ages)           # measurement 2, table of fitted lambdas
# correction_share(ages)       # share of the density, not of the estimate
