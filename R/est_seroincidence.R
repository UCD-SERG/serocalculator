#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' This function models seroincidence using maximum likelihood estimation;
#' that is, it finds the value of the seroincidence parameter which
#' maximizes the likelihood (i.e., joint probability) of the data.
#' @inheritParams log_likelihood
#' @inheritParams stats::nlm
#' @param pop_data a [data.frame] with cross-sectional serology data per
#' antibody and age, and additional columns
#' @param lambda_start starting guess for incidence rate, in events/year.
#' @param antigen_isos Character vector with one or more antibody names.
#' Must match `pop_data`
#' @param build_graph whether to graph the log-likelihood function across
#' a range of incidence rates (lambda values)
#' @param print_graph whether to display the log-likelihood curve graph
#' in the course of running `est_seroincidence()`
#' @param stepmin A positive scalar providing the minimum allowable
#' relative step length.
#' @param sr_params a [data.frame()] containing MCMC samples of parameters
#' from the Bayesian posterior distribution of a longitudinal decay curve model.
#' The parameter columns must be named:
#' - `antigen_iso`: a [character()] vector indicating antigen-isotype
#' combinations
#' - `iter`: an [integer()] vector indicating MCMC sampling iterations
#' - `y0`: baseline antibody level at $t=0$ ($y(t=0)$)
#' - `y1`: antibody peak level (ELISA units)
#' - `t1`: duration of infection
#' - `alpha`: antibody decay rate
#' (1/days for the current longitudinal parameter sets)
#' - `r`: shape factor of antibody decay
#' @param cluster_var optional name(s) of the variable(s) in `pop_data`
#' containing cluster identifiers for clustered sampling designs
#' (e.g., households, schools).
#' Can be a single variable name (character string) or a vector of
#' variable names for multi-level clustering (e.g., `c("school",
#' "classroom")`). When provided, standard errors will be adjusted for
#' within-cluster correlation using cluster-robust variance estimation.
#' Note: ICC calculation via `compute_icc()` only supports
#' single-level clustering.
#' @param stratum_var optional name of the variable in `pop_data` containing
#' stratum identifiers. Used in combination with `cluster_var` for
#' stratified cluster sampling designs.
#' @param sampling_weights optional [data.frame] containing sampling
#' weights with columns for cluster/stratum identifiers and their sampling
#' probabilities. Currently not implemented; reserved for future use.
#' @inheritDotParams stats::nlm -f -p -hessian -print.level -steptol

#' @returns a `"seroincidence"` object, which is a [stats::nlm()] fit object
#' with extra metadata attributes `lambda_start`, `antigen_isos`, and `ll_graph`
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' sr_curve <-
#'   typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' # Basic usage without clustering
#' est1 <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = sr_curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#' )
#'
#' summary(est1)
#'
#' # Usage with clustered sampling design
#' # Standard errors will be adjusted for within-cluster correlation
#' est2 <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = sr_curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   cluster_var = "cluster"
#' )
#'
#' summary(est2)
#'
#' # With both cluster and stratum variables
#' est3 <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = sr_curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   cluster_var = "cluster",
#'   stratum_var = "catchment"
#' )
#'
#' summary(est3)
est_seroincidence <- function(
    pop_data,
    sr_params,
    noise_params,
    antigen_isos = get_biomarker_names(pop_data),
    lambda_start = 0.1,
    stepmin = 1e-8,
    stepmax = 3,
    verbose = FALSE,
    build_graph = FALSE,
    print_graph = build_graph & verbose,
    cluster_var = NULL,
    stratum_var = NULL,
    sampling_weights = NULL,
    ...) {
  if (verbose > 1) {
    cli::cli_inform("inputs to `est_seroincidence()`:")
    print(environment() |> as.list())
  }

  # Validate cluster/stratum parameters
  if (!is.null(sampling_weights)) {
    cli::cli_warn(
      "{.arg sampling_weights} is not yet implemented and will be ignored."
    )
  }

  if (!is.null(cluster_var)) {
    # Check all cluster variables exist in pop_data
    missing_vars <- setdiff(cluster_var, names(pop_data))
    if (length(missing_vars) > 0) {
      cli::cli_abort(c(
        "x" = paste(
          "{.arg cluster_var} = {.val {missing_vars}}",
          "is not a column in {.arg pop_data}."
        )
      ))
    }
  }

  if (!is.null(stratum_var)) {
    if (!stratum_var %in% names(pop_data)) {
      cli::cli_abort(c(
        "x" = paste(
          "{.arg stratum_var} = {.val {stratum_var}}",
          "is not a column in {.arg pop_data}."
        )
      ))
    }
  }

  .error_check(
    data = pop_data,
    antigen_isos = antigen_isos,
    curve_params = sr_params
  )

  # Prepare columns to keep
  cols_to_keep <- c(
    pop_data |> get_values_var(),
    pop_data |> get_age_var(),
    "antigen_iso"
  )

  # Add cluster/stratum variables if specified
  if (!is.null(cluster_var)) {
    cols_to_keep <- c(cols_to_keep, cluster_var)
  }
  if (!is.null(stratum_var)) {
    cols_to_keep <- c(cols_to_keep, stratum_var)
  }

  pop_data <- pop_data |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select(dplyr::all_of(cols_to_keep)) |>
    filter(if_all(everything(), ~!is.na(.x)))

  sr_params <- sr_params |>
    ungroup() |>
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1
    ) |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select("y1", "alpha", "d", "antigen_iso") |>
    droplevels()

  noise_params <- noise_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    droplevels()

  # incidence can not be calculated if there are zero observations.
  if (nrow(pop_data) == 0) {
    cli::cli_abort("No data provided.")
  }

  if (verbose) {
    cli::cli_inform(c(i = "nrow(sr_params) = {nrow(sr_params)}"))
  }

  if (nrow(noise_params) != length(antigen_isos)) {
    cli::cli_abort("too many rows of noise parameters.")
  }

  pop_data <- pop_data |> split(~antigen_iso)
  sr_params <- sr_params |> split(~antigen_iso)
  noise_params <- noise_params |> split(~antigen_iso)

  # First, check if we find numeric results...
  res <- .nll(
    pop_data = pop_data,
    log.lambda = log(lambda_start),
    antigen_isos = antigen_isos,
    curve_params = sr_params,
    noise_params = noise_params,
    verbose = verbose,
    ...
  )

  if (is.na(res)) {
    cli::cli_warn(
      "Could not calculate log-likelihood with starting parameter value."
    )
    return(NULL)
  }

  if (verbose) {
    cli::cli_inform("Initial negative log-likelihood: {res}")
  }

  if (build_graph) {
    if (verbose) cli::cli_inform("building likelihood graph")
    graph <- graph_loglik(
      highlight_points = lambda_start,
      highlight_point_names = "lambda_start",
      pop_data = pop_data,
      antigen_isos = antigen_isos,
      curve_params = sr_params,
      noise_params = noise_params
    )
    if (print_graph) {
      print(
        graph +
          ggplot2::scale_x_log10(
            labels = scales::label_comma()
          )
      )
    }
  } else {
    graph <- NULL
  }


  # [stats::nlm()] expects an objective function `f`
  # "returning a single numeric value",
  # but [.nll()] is vectorized via its subfunction [f_dev()].
  # The vectorization doesn't appear to cause a problem for [nlm()].

  if (verbose) cli::cli_inform("about to call `nlm()`")
  # Estimate lambda
  time <- system.time(
    {
      fit <- nlm(
        f = .nll,
        p = log(lambda_start),
        pop_data = pop_data,
        antigen_isos = antigen_isos,
        curve_params = sr_params,
        noise_params = noise_params,
        hessian = TRUE,
        stepmax = stepmax,
        steptol = stepmin,
        verbose = verbose,
        print.level = ifelse(verbose, 2, 0),
        ...
      )
    }
  )

  code_text <- nlm_exit_codes[fit$code]
  message1 <- "\n`nlm()` completed with the following convergence code:\n"
  if (fit$code %in% 3:5) {
    cli::cli_warn(
      c(
        "`nlm()` may not have reached the maximum likelihood estimate.",
        message1,
        code_text
      )
    )
  }

  if (verbose >= 2) {
    cli::cli_inform("\nElapsed time: ")
    print(time)
  }

  if (build_graph) {
    graph <-
      graph |>
      add_point_to_graph(
        fit = fit,
        pop_data = pop_data,
        antigen_isos = antigen_isos,
        curve_params = sr_params,
        noise_params = noise_params
      )

    if (print_graph) {
      print(
        graph +
          ggplot2::scale_x_log10(
            labels = scales::label_comma()
          )
      )
    }
  }

  # Store clustering-related attributes only if clustering is being used
  if (!is.null(cluster_var)) {
    fit <- fit |>
      structure(
        class = union("seroincidence", class(fit)),
        lambda_start = lambda_start,
        antigen_isos = antigen_isos,
        ll_graph = graph,
        cluster_var = cluster_var,
        stratum_var = stratum_var,
        pop_data = pop_data,
        sr_params = sr_params,
        noise_params = noise_params
      )
  } else {
    fit <- fit |>
      structure(
        class = union("seroincidence", class(fit)),
        lambda_start = lambda_start,
        antigen_isos = antigen_isos,
        ll_graph = graph
      )
  }

  return(fit)
}

#' @title Estimate Seroincidence
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `est.incidence()` was renamed to [est_seroincidence()] to create a more
#' consistent API.
#' @keywords internal
#' @export
est.incidence <- function( # nolint: object_name_linter
    ...) {
  lifecycle::deprecate_soft("1.3.1", "est.incidence()", "est_seroincidence()")
  est_seroincidence(
    ...
  )
}
