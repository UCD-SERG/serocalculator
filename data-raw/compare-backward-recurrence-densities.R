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
# Both densities defined below are conditional on having seroconverted at
# least once. The never-infected branch has mass exp(-lambda * age) under
# both forms, so it says nothing about which is right; the whole
# difference is in the shape over [0, age].


# ---- where the two forms come from -----------------------------------
#
# `dens_from_birth` below is exact for a homogeneous Poisson process of
# rate lambda running on [0, age]. Writing T for the time since the most
# recent event, and noting that T is undefined when no event occurred:
#
#   P(T > t, at least one event) = exp(-lambda * t) - exp(-lambda * age)
#
# That is a sub-survivor rather than a survivor function: at t = 0 it
# equals 1 - exp(-lambda * age), the probability of any event at all,
# not 1. Differentiating gives lambda * exp(-lambda * t) on [0, age],
# with the remaining exp(-lambda * age) on "no event ever occurred".
# Dividing by 1 - exp(-lambda * age) conditions on having seroconverted,
# which is the form implemented below.
#
# `dens_teunis_2020` below comes from a different construction.
# Appendix A of that paper assumes the subject sits at a uniformly random
# point of the inter-infection interval containing them, truncated at
# their age:
#
#   u_f(tau | dt, age) = [0 <= tau <= min(dt, age)] / min(dt, age)
#
# and integrates that against a length-biased interval distribution. For
# the exponential interval law assumed throughout that paper, the
# length-biased density is lambda^2 * dt * exp(-lambda * dt), and the
# integral yields the closed form implemented below. The `min(dt, age)`
# is what produces the extra term: when the interval is longer than the
# subject's life, the most recent infection is placed uniformly during
# that life rather than derived.
#
# The exact relation between the two is a mixture, not a dropped term:
#
#   dens_teunis_2020 = (1 - exp(-lambda * a)) * dens_from_birth
#                      + exp(-lambda * a) * Uniform(0, a)
#
# So exp(-lambda * a) is the uniform component's mixture weight, which is
# what `correction_share()` reports and what `sim_teunis_2020()` draws.
# Deleting the extra term from `dens_teunis_2020` does NOT give the
# article's density: what is left integrates to 1 - exp(-lambda * a)
# rather than to 1, because the two also differ by that normalization.
#
# The two forms are not distinguishable by any normalization check. Both
# integrate to 1 - exp(-lambda * age) over [0, age] once scaled into the
# full mixture, and both leave exp(-lambda * age) for the never-infected
# branch. Only the shape over [0, age] differs, the 2020 form being
# flatter.
#
# What the measurements below establish: `dens_from_birth` reproduces a
# directly simulated Poisson process to three decimals and
# `dens_teunis_2020` does not, so for the constant-rate model the package
# states, the article's form is exact and the engine's is an
# approximation. Fitting one to the other's data moves the estimate by
# about 1.6 to 3.4 percent over the SEES age distribution.
#
# Two mechanisms hold that difference down, at opposite ends of the
# range, and it is worth not attributing both to one. At low lambda * age
# the shared never-infected atom carries most of the Fisher information
# about lambda (about 96 percent at lambda = 0.05 over these ages, about
# 86 percent at 0.10), so the disagreement over shape has little to work
# with. At high lambda * age that is no longer true (about 27 percent at
# lambda = 0.5), but the two densities have themselves converged, since
# the uniform component's weight exp(-lambda * a) has gone to zero.
#
# Neither form is therefore "the wrong formula". The 2020 construction
# generalizes conveniently to non-exponential inter-infection intervals.
# The from-birth derivation covers that case too, as the backward
# recurrence time of an ordinary renewal process started at birth, but
# has no closed form for a general interval law; it pays for its
# exactness in tractability, not in scope.


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

# Both families are keyed on the same names throughout, so a third
# candidate is added in one place rather than three.
densities <- list(
  from_birth = dens_from_birth,
  teunis_2020 = dens_teunis_2020
)


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

simulators <- list(
  from_birth = sim_from_birth,
  teunis_2020 = sim_teunis_2020
)


# ---- seeding without clobbering the caller's stream ------------------

# `set.seed()` alone would overwrite the caller's RNG state, which the
# lab's restore-global-state rule forbids. `withr::local_seed()` does
# this too, but `withr` is only in Suggests, so this stays base-only and
# runnable wherever the script is sourced.
with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = globalenv())
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(
      assign(".Random.seed", old_seed, envir = globalenv()),
      add = TRUE
    )
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }
  set.seed(seed)
  code
}


# ---- estimation ------------------------------------------------------

# Negative log-likelihood for directly observed times since infection.
# `times` is NA for subjects with no seroconversion; `ages` must be the
# same length. `model` names an entry of `densities`.
nll_time_since_infection <- function(lambda, times, ages, model) {
  model <- match.arg(model, names(densities))
  if (length(times) != length(ages)) {
    stop(
      "`times` and `ages` must be the same length; got ",
      length(times), " and ", length(ages), "."
    )
  }
  if (lambda <= 0) {
    return(Inf)
  }
  infected <- !is.na(times)
  never_infected_term <- sum(-lambda * ages[!infected])
  density <- densities[[model]](times[infected], lambda, ages[infected])
  # Both branches are stated conditional on infection, so scale by the
  # probability of being in that branch to get the full mixture.
  infected_term <- sum(
    log(1 - exp(-lambda * ages[infected])) + log(density)
  )
  -(never_infected_term + infected_term)
}

# `stats::optimize` gives no signal when it converges to an endpoint, so
# a lambda outside `interval` would be reported as the boundary value
# with nothing to distinguish it from a real fit. Refuse instead.
fit_lambda <- function(times, ages, model, interval = c(1e-4, 3)) {
  fit <- stats::optimize(
    nll_time_since_infection,
    interval = interval,
    times = times,
    ages = ages,
    model = model
  )
  margin <- 1e-3 * diff(interval)
  if (min(abs(fit$minimum - interval)) < margin) {
    stop(
      "`fit_lambda` converged to the edge of `interval` (",
      signif(fit$minimum, 6), " against [",
      interval[1], ", ", interval[2],
      "]); the optimum is probably outside it. Widen `interval`."
    )
  }
  fit$minimum
}


# ---- measurement 1: which density matches the Poisson process --------

# Simulate from the process itself and compare the empirical conditional
# density against both candidates. Returns one row per histogram bin.
compare_densities <- function(lambda = 0.1,
                              age = 10,
                              n_draws = 4e5,
                              bin_width = 1,
                              seed = 1) {
  with_seed(seed, {
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
  })
}


# ---- measurement 2: what the choice costs in lambda-hat --------------

# Fit both models to data generated by each, over a supplied age
# distribution. Each model should recover its own truth; the off-diagonal
# rows are the misspecification bias.
#
# `n_copies` tiles the age vector to set the sample size. It does NOT
# replicate the experiment: each cell is a single fit to one pooled
# sample, so this table carries no Monte Carlo error. Use
# `compare_fits_mc()` for that.
compare_fits <- function(ages,
                         lambdas = c(0.05, 0.1, 0.2, 0.5),
                         n_copies = 40,
                         seed = 2) {
  with_seed(seed, {
    pooled_ages <- rep(ages, n_copies)
    rows <- lapply(lambdas, function(lambda) {
      lapply(names(simulators), function(generator) {
        simulate_one <- simulators[[generator]]
        times <- vapply(
          pooled_ages,
          function(age) simulate_one(lambda, age),
          numeric(1)
        )
        fits <- vapply(
          names(densities),
          function(model) fit_lambda(times, pooled_ages, model),
          numeric(1)
        )
        data.frame(
          true_lambda = lambda,
          generated_by = generator,
          fit_from_birth = fits[["from_birth"]],
          fit_teunis_2020 = fits[["teunis_2020"]]
        )
      })
    })
    fits <- do.call(rbind, unlist(rows, recursive = FALSE))
    fits$ratio <- fits$fit_teunis_2020 / fits$fit_from_birth
    fits
  })
}

# Re-run `compare_fits()` across independent seeds and report the Monte
# Carlo spread of the ratio, so a reader can tell the measured bias from
# noise. Keep `n_copies` small here: this is a whole re-fit per seed.
compare_fits_mc <- function(ages,
                            lambdas = c(0.05, 0.1, 0.2, 0.5),
                            n_copies = 5,
                            seeds = 1:8) {
  per_seed <- lapply(
    seeds,
    function(seed) {
      compare_fits(
        ages,
        lambdas = lambdas,
        n_copies = n_copies,
        seed = seed
      )
    }
  )
  stacked <- do.call(rbind, per_seed)
  # Group on a composite key rather than on `split()`'s generated names:
  # a lambda of 0.05 puts a "." inside the name, so parsing the name back
  # apart would split the number.
  key <- paste(stacked$true_lambda, stacked$generated_by, sep = "|")
  cells <- split(stacked$ratio, key)
  first <- !duplicated(key)
  out <- stacked[first, c("true_lambda", "generated_by")]
  cells <- cells[key[first]]
  standard_error <- function(x) stats::sd(x) / sqrt(length(x))
  out$mean_ratio <- vapply(cells, mean, numeric(1))
  out$se_ratio <- vapply(cells, standard_error, numeric(1))
  out$n_seeds <- lengths(cells)
  row.names(out) <- NULL
  out
}


# ---- weight of the uniform component ---------------------------------

# The uniform component of `dens_teunis_2020` integrates to
# exp(-lambda * age) over [0, age], so that is its mixture weight -- the
# share of the infected-branch density the article's form distributes
# differently.
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
# compare_fits_mc(ages)        # Monte Carlo error on measurement 2
# correction_share(ages)       # mixture weight, not a share of the estimate
