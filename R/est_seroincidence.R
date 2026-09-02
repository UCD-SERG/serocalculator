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
#' When fitting more than one `antigen_isos` at once, this argument
#' also has a second use.
#' `log_likelihood()` combines biomarkers by summing their marginal
#' log-likelihoods, which is only valid if those contributions are
#' independent.
#' Two biomarker readings from the same person usually aren't, since
#' they share an infection history.
#' Pass the id column returned by [ids_varname()]
#' (e.g. `cluster_var = ids_varname(pop_data)`) to get a
#' cluster-robust standard error that accounts for this within-person
#' correlation.
#' This is a distinct concern from a genuine sampling-cluster design,
#' and multiple `cluster_var` values are grouped by their
#' intersection rather than combined additively: if every subject
#' belongs to exactly one sampling cluster,
#' `cluster_var = c("cluster", "id")` reduces to clustering by `id`
#' alone, and does not add the sampling-cluster correction on top ---
#' see issue [#543](https://github.com/UCD-SERG/serocalculator/issues/543).
#' See issue [#645](https://github.com/UCD-SERG/serocalculator/issues/645).
#' @param stratum_var optional name of the variable in `pop_data` containing
#' stratum identifiers. Used in combination with `cluster_var` for
#' stratified cluster sampling designs.
#' @param sampling_weights optional [data.frame] containing sampling
#' weights with columns for cluster/stratum identifiers and their sampling
#' probabilities. Currently not implemented; reserved for future use.
#' @param method how to combine several biomarkers into one likelihood;
#' see [log_likelihood()]. `"composite"` (the default) sums the
#' per-biomarker marginal log-likelihoods; `"joint"` integrates over a
#' shared latent infection time per person, which needs `pop_data` to
#' identify people (see [ids_varname()]).
#' The two methods call for different standard errors: the composite
#' likelihood's naive standard error assumes independence across
#' biomarkers (pass `cluster_var = ids_varname(pop_data)` to correct it),
#' while the joint likelihood is a genuine likelihood, so its
#' Hessian-based standard error is valid as is
#' (up to any sampling-design clustering, which `cluster_var` still
#' handles).
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
#'
#' # Shared-latent-time (joint) likelihood across the two biomarkers
#' est4 <- est_seroincidence(
#'   pop_data = xs_data,
#'   sr_params = sr_curve,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   method = "joint"
#' )
#'
#' summary(est4)
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
  method = c("composite", "joint"),
  ...
) {
  method <- rlang::arg_match(method)

  if (verbose > 1) {
    cli::cli_inform("inputs to `est_seroincidence()`:")
    print(environment() |> as.list())
  }

  # Validate cluster/stratum parameters
  .validate_cluster_params(
    pop_data = pop_data,
    cluster_var = cluster_var,
    stratum_var = stratum_var,
    sampling_weights = sampling_weights
  )

  .error_check(
    data = pop_data,
    antigen_isos = antigen_isos,
    curve_params = sr_params
  )

  .warn_biomarker_independence(
    pop_data = pop_data,
    antigen_isos = antigen_isos,
    cluster_var = cluster_var,
    verbose = verbose,
    method = method
  )

  # Prepare columns to keep
  cols_to_keep <- c(
    pop_data |> get_values_var(),
    pop_data |> get_age_var(),
    "antigen_iso"
  )

  # The joint likelihood pairs each person's biomarker readings, so it
  # needs the id column, which the composite likelihood never looks at.
  id_var <- NULL
  if (method == "joint") {
    id_var <- .joint_id_var_for_fit(pop_data, antigen_isos)
    cols_to_keep <- c(cols_to_keep, id_var)
  }

  # Add cluster/stratum variables if specified
  if (!is.null(cluster_var)) {
    cols_to_keep <- c(cols_to_keep, cluster_var)
  }
  if (!is.null(stratum_var)) {
    cols_to_keep <- c(cols_to_keep, stratum_var)
  }

  pop_data <- pop_data |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select(dplyr::all_of(unique(cols_to_keep))) |>
    filter(if_all(everything(), ~!is.na(.x)))

  # `iter` (and `chain`) let the joint likelihood pair posterior draws
  # across biomarkers; the composite likelihood evaluates each biomarker's
  # draws separately, so it has no use for them.
  draw_id_cols <- if (method == "joint") c("iter", "chain") else character(0)

  sr_params <- sr_params |>
    ungroup() |>
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1
    ) |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select(
      "y1", "alpha", "d", "antigen_iso",
      dplyr::any_of(draw_id_cols)
    ) |>
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
    method = method,
    id_var = id_var,
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

  if (method == "joint" && length(antigen_isos) > 1) {
    .warn_joint_dropped_subjects(
      pop_data = pop_data,
      curve_params = sr_params,
      noise_params = noise_params,
      antigen_isos = antigen_isos,
      id_var = id_var
    )
  }

  if (build_graph) {
    if (verbose) cli::cli_inform("building likelihood graph")
    graph <- graph_loglik(
      highlight_points = lambda_start,
      highlight_point_names = "lambda_start",
      pop_data = pop_data,
      antigen_isos = antigen_isos,
      curve_params = sr_params,
      noise_params = noise_params,
      method = method,
      id_var = id_var
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
        method = method,
        id_var = id_var,
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
        noise_params = noise_params,
        method = method,
        id_var = id_var
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

  # Recorded only for the joint likelihood, so that fits under the
  # default method are unchanged (including their snapshots).
  if (method == "joint") {
    attr(fit, "method") <- method
    attr(fit, "id_var") <- id_var
  }

  return(fit)
}

#' Warn when the joint likelihood drops subjects
#'
#' @inheritParams f_dev_joint
#' @returns `invisible(NULL)`
#' @noRd
.warn_joint_dropped_subjects <- function(
  pop_data,
  curve_params,
  noise_params,
  antigen_isos,
  id_var
) {
  dropped <- .joint_dropped_subjects(
    pop_data = pop_data,
    curve_params = curve_params,
    noise_params = noise_params,
    antigen_isos = antigen_isos,
    id_var = id_var
  )
  if (dropped$n_dropped == 0) {
    return(invisible(NULL))
  }
  cli::cli_warn(
    c(
      "{dropped$n_dropped} of {dropped$n_subjects} subjects have zero
      likelihood under every posterior draw and contribute nothing to the
      joint likelihood.",
      "i" = "For these subjects, no shared time since infection is
      compatible with all of their biomarker readings under the noise
      model ({.field nu}, {.field eps}), so the estimate is based on the
      remaining subjects only. If the dropped subjects tend to have high
      readings (recent infections), the incidence estimate will be biased
      downward.",
      "i" = "Consider whether the noise parameters are realistic for these
      biomarkers, or use {.code method = \"composite\"}, which evaluates
      each biomarker on its own."
    ),
    class = "joint_dropped_subjects"
  )
  invisible(NULL)
}

#' Resolve, and sanity-check, the subject id column for a joint fit
#'
#' @param pop_data the (unfiltered) `pop_data` passed to
#' [est_seroincidence()]
#' @param antigen_isos the biomarkers being combined
#' @returns the id column name
#' @noRd
.joint_id_var_for_fit <- function(pop_data, antigen_isos) {
  id_var <- tryCatch(
    suppressWarnings(ids_varname(pop_data)),
    error = function(e) NULL
  )
  if (is.null(id_var)) {
    id_var <- .joint_id_var(pop_data)
  }

  if (length(antigen_isos) > 1) {
    relevant <- pop_data[pop_data$antigen_iso %in% antigen_isos, , drop = FALSE]
    n_bio_per_id <- tapply(
      relevant$antigen_iso,
      relevant[[id_var]],
      FUN = \(x) length(unique(x))
    )
    if (all(n_bio_per_id < 2)) {
      cli::cli_warn(
        c(
          "No subject in {.arg pop_data} has readings for more than one
          of {.val {antigen_isos}}, so the joint likelihood reduces to the
          composite one.",
          "i" = "Check that the id column ({.val {id_var}}) identifies
          people rather than observations; see {.fun set_id_var}."
        )
      )
    }
  }

  id_var
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
  ...
) {
  lifecycle::deprecate_soft("1.3.1", "est.incidence()", "est_seroincidence()")
  est_seroincidence(
    ...
  )
}
