#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' This function models seroincidence using maximum likelihood estimation;
#' that is, it finds the value of the seroincidence parameter which
#' maximizes the likelihood (i.e., joint probability) of the data.
#' @inheritParams log_likelihood
#' @inheritParams stats::nlm
#' @param pop_data a [data.frame] with cross-sectional serology data per
#' antibody and age, and additional columns
#' @param lambda_start starting guess for incidence rate, in years/event.
#' @param antigen_isos Character vector with one or more antibody names.
#' Must match `pop_data`
#' @param build_graph whether to graph the log-likelihood function across
#' a range of incidence rates (lambda values)
#' @param print_graph whether to display the log-likelihood curve graph
#' in the course of running `estimate_scr()`
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
#' est1 <- estimate_scr(
#'   pop_data = xs_data,
#'   sr_params = sr_curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#' )
#'
#' summary(est1)
estimate_scr <- function(
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
    ...) {
  if (verbose > 1) {
    message("inputs to `estimate_scr()`:")
    print(environment() |> as.list())
  }

  .errorCheck(
    data = pop_data,
    antigen_isos = antigen_isos,
    curve_params = sr_params
  )

  pop_data <- pop_data |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select(
      pop_data |> get_values_var(),
      pop_data |> get_age_var(),
      "antigen_iso"
    ) |>
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
    stop("No data provided.")
  }

  if (verbose) {
    message("nrow(sr_params) = ", nrow(sr_params))
  }

  if (nrow(noise_params) != length(antigen_isos)) {
    stop("too many rows of noise parameters.")
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
    warning("Could not calculate log-likelihood with starting parameter value.")
    return(NULL)
  }

  if (verbose) {
    message("Initial negative log-likelihood: ", res)
  }

  if (build_graph) {
    if (verbose) message("building likelihood graph")
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

  if (verbose) message("about to call `nlm()`")
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
    warning(
      "`nlm()` may not have reached the maximum likelihood estimate.",
      message1,
      code_text
    )
  }

  if (verbose >= 2) {
    message("\nElapsed time: ")
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

  fit <- fit |>
    structure(
      class = union("seroincidence", class(fit)),
      lambda_start = lambda_start,
      antigen_isos = antigen_isos,
      ll_graph = graph
    )

  return(fit)
}

#' @title Estimate Seroincidence
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `est.incidence()` was renamed to [estimate_scr()] to create a more
#' consistent API.
#' @keywords internal
#' @export
est.incidence <- function( # nolint: object_name_linter
    ...) {
  lifecycle::deprecate_soft("1.3.1", "est.incidence()", "estimate_scr()")
  estimate_scr(
    ...
  )
}
