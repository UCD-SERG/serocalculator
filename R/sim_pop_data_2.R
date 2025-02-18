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
#' @inheritDotParams simcs.tinf
#' @inheritParams log_likelihood # verbose
#' @return a [tibble::tbl_df] containing simulated cross-sectional serosurvey
#' data, with columns:
#'
#' * `age`: age (in days)
#' * one column for each element in the `antigen_iso` input argument
#'
#' @export
#' @keywords internal
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
#' lambda <- 0.2
#' # Range covered in simulations
#' lifespan <- c(0, 10)
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
#' csdata <- sim_pop_data_2(
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
sim_pop_data_2 <- function(
    lambda = 0.1,
    n_samples = 100,
    age_range = c(0, 20),
    age_fixed = NA,
    antigen_isos = intersect(
      get_biomarker_levels(curve_params),
      rownames(noise_limits)
    ),
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
    print(environment() |> as.list())
  }

  chain_in_curve_params <- "chain" %in% names(curve_params)
  pop_data <- tibble::tibble(
    id = seq_len(n_samples) |> as.character(),
    age = sim_age(
      n_samples = n_samples,
      age_range = age_range
    ),
    time_since_last_seroconversion =
      sim_time_since_last_sc(
        lambda = lambda,
        n_samples = n_samples,
        age = age
      ),

    mcmc_iter = sample(
      size = n_samples,
      x = curve_params$iter,
      replace = TRUE
    ),
    mcmc_chain = # nolint: object_usage_linter
      if (chain_in_curve_params) {
        sample(
          size = n_samples,
          x = curve_params$chain,
          replace = TRUE
        )
      }
  ) |>
    reframe(
      .by = everything(),
      antigen_iso = antigen_isos
    ) |>
    left_join(
      curve_params,
      relationship = "many-to-one",
      by =
        c(
          "antigen_iso",
          "mcmc_iter" = "iter",
          if (chain_in_curve_params) "mcmc_chain" = "chain"
        )
    ) |>
    mutate(
      `E[Y]` = ab_5p(
        t = .data$time_since_last_seroconversion,
        y0 = .data$y0,
        y1 = .data$y1,
        t1 = .data$t1,
        alpha = .data$alpha,
        shape = .data$r
      )
    )

  if (add_noise) {

    pop_data <- pop_data |>
      mutate(
        noise = runif(
          n = dplyr::n(),
          min = noise_limits[.data$antigen_iso, "min"],
          max = noise_limits[.data$antigen_iso, "max"]
        ),
        Y = .data$`E[Y]` + noise
      )
  } else {
    pop_data <- pop_data |>
      mutate(Y = .data$`E[Y]`)
  }

  pop_data <- pop_data |>
    select(all_of(c("id", "age", "antigen_iso", "Y"))) |>
    structure(
      format = "long"
    ) |>
    as_pop_data(
      value = "Y",
      age = "age",
      id = "id"
    )

  if (format == "wide") {
    cli::cli_abort("`format = 'wide' not yet implemented.")
  }

  return(pop_data)
}
