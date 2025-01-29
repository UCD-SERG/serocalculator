#' Simulate a cross-sectional serosurvey with noise
#'
#' @description
#' Makes a cross-sectional data set (age, y(t) set)
#'  and adds noise, if desired.

#' @param lambda a [numeric()] scalar indicating the incidence rate
#' (in events per person-years)
#' @param n_samples number of samples to simulate
#' @param age_range age range of sampled individuals, in years
#' @param age_fixed specify the curve parameters to use by age
#' (does nothing at present?)
#' @param antigen_isos Character vector with one or more antibody names.
#' Values must match `curve_params`.
#' @param n_mcmc_samples how many MCMC samples to use:
#' * when `n_mcmc_samples` is in `1:4000` a fixed posterior sample is used
#' * when `n_mcmc_samples` = `0`, a random sample is chosen
#' @param renew_params whether to generate a new parameter set for each
#' infection
#' * `renew_params = TRUE` generates a new parameter set for each infection
#' * `renew_params = FALSE` keeps the one selected at birth,
#' but updates baseline y0
#' @param add_noise a [logical()] indicating
#' whether to add biological and measurement noise
#' @inheritParams log_likelihood

#' @param noise_limits biologic noise distribution parameters
#' @param format a [character()] variable, containing either:
#' * `"long"` (one measurement per row) or
#' * `"wide"` (one serum sample per row)
#' @param ... additional arguments passed to `simcs.tinf()`
#' @inheritDotParams simcs.tinf
#' @inheritParams log_likelihood # verbose
#' @return a [tibble::tbl_df] containing simulated cross-sectional serosurvey
#' data, with columns:
#'
#' * `age`: age (in days)
#' * one column for each element in the `antigen_iso` input argument
#'
#' @export
#' @examples
#' # Load curve parameters
#' dmcmc <- typhoid_curves_nostrat_100
#'
#' # Specify the antibody-isotype responses to include in analyses
#' antibodies <- c("HlyE_IgA", "HlyE_IgG")
#'
#' # Set seed to reproduce results
#' set.seed(54321)
#'
#' # Simulated incidence rate per person-year
#' lambda <- 0.2;
#'
#' # Range covered in simulations
#' lifespan <- c(0, 10);
#'
#' # Cross-sectional sample size
#' nrep <- 100
#'
#' # Biologic noise distribution
#' dlims <- rbind(
#'   "HlyE_IgA" = c(min = 0, max = 0.5),
#'   "HlyE_IgG" = c(min = 0, max = 0.5)
#' )
#'
#' # Generate cross-sectional data
#' csdata <- sim_pop_data(
#'   curve_params = dmcmc,
#'   lambda = lambda,
#'   n_samples = nrep,
#'   age_range = lifespan,
#'   antigen_isos = antibodies,
#'   n_mcmc_samples = 0,
#'   renew_params = TRUE,
#'   add_noise = TRUE,
#'   noise_limits = dlims,
#'   format = "long"
#' )
#'
sim_pop_data <- function(
    lambda = 0.1,
    n_samples = 100,
    age_range = c(0, 20),
    age_fixed = NA,
    antigen_isos = intersect(
      get_biomarker_levels(curve_params),
      rownames(noise_limits)),
    n_mcmc_samples = 0,
    renew_params = FALSE,
    add_noise = FALSE,
    curve_params,
    noise_limits,
    format = "wide",
    verbose = FALSE,
    ...) {
  if (verbose > 1) {
    message("inputs to `sim_pop_data()`:")
    print(environment() %>% as.list())
  }



  # predpar is an [array()] containing MCMC samples from the Bayesian distribution
  # of longitudinal decay curve model parameters.
  # NOTE: most users should leave `predpar` at its default value and provide
  # `curve_params` instead.

  predpar <-
    curve_params %>%
    filter(.data$antigen_iso %in% antigen_isos) %>%
    droplevels() %>%
    prep_curve_params_for_array() %>%
    df_to_array(dim_var_names = c("antigen_iso", "parameter"))

  stopifnot(length(lambda) == 1)

  day2yr <- 365.25
  lambda <- lambda / day2yr
  age_range <- age_range * day2yr
  npar <- dimnames(predpar)$parameter %>% length()


  baseline_limits <- noise_limits

  ysim <- simcs.tinf(
    lambda = lambda,
    n_samples = n_samples,
    age_range = age_range,
    age_fixed = age_fixed,
    antigen_isos = antigen_isos,
    n_mcmc_samples = n_mcmc_samples,
    renew_params = renew_params,
    predpar = predpar,
    blims = baseline_limits,
    npar = npar,
    ...
  )

  if (add_noise) {
    for (k.ab in 1:(ncol(ysim) - 1)) {
      ysim[, 1 + k.ab] <-
        ysim[, 1 + k.ab] +
        runif(
          n = nrow(ysim),
          min = noise_limits[k.ab, 1],
          max = noise_limits[k.ab, 2]
        )
    }
  }
  colnames(ysim) <- c("age", antigen_isos)

  to_return <-
    ysim %>%
    as_tibble() %>%
    mutate(
      id = as.character(row_number()),
      age = round(.data$age / day2yr, 2)
    )

  if (format == "long") {
    if (verbose) message("outputting long format data")
    to_return <-
      to_return %>%
      pivot_longer(
        cols = all_of(antigen_isos),
        values_to = c("value"),
        names_to = c("antigen_iso")
      ) %>%
      structure(
        class = c("pop_data", class(to_return)),
        format = "long"
      ) %>%
      set_value(value = "value") %>%
      set_age(age = "age") %>%
      set_id(id = "id")
  } else {
    if (verbose) message("outputting wide format data")
    to_return <-
      to_return %>%
      structure(
        class = c("pop_data_wide", class(to_return)),
        format = "wide"
      )
  }

  return(to_return)
}

#' @title Simulate a cross-sectional serosurvey with noise
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `sim.cs()` was renamed to [sim_pop_data()] to create a more
#' consistent API.
#'
#' @keywords internal
sim.cs <- function(...) { # nolint: object_name_linter
  lifecycle::deprecate_soft("1.3.1", "sim.cs()", "sim_pop_data()")
  sim_pop_data(...)
}
