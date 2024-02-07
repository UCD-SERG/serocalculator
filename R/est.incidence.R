#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' This function models seroincidence using maximum likelihood estimation; that is, it finds the value of the seroincidence parameter which maximizes the likelihood (i.e., joint probability) of the data.
#' @inheritParams llik
#' @inheritParams stats::nlm
#' @param pop_data [data.frame()] with cross-sectional serology data per antibody and age, and additional columns
#' @param lambda_start starting guess for incidence rate, in years/event.
#' @param antigen_isos Character vector with one or more antibody names. Values must match `pop_data`
#' @param build_graph whether to graph the log-likelihood function across a range of incidence rates (lambda values)
#' @param print_graph whether to display the log-likelihood curve graph in the course of running `est.incidence()`
#' @param stepmin A positive scalar providing the minimum allowable relative step length.
#' @inheritDotParams stats::nlm -f -p -hessian -print.level -steptol

#' @returns a `"seroincidence"` object, which is a [stats::nlm()] fit object with extra meta-data attributes `lambda_start`, `antigen_isos`, and `ll_graph`
#' @export
#' @examples
#' xs_data = load_pop_data(""https://osf.io/download//n6cp3/"")
#' curve = load_curve_params(""https://osf.io/download/rtw5k/"" )
#'
#' est1 = est.incidence(
#' pop_data = xs_data %>% filter(Country == "Pakistan"),
#' curve_param = curve,
#' noise_param = noise %>% filter(Country == "Pakistan"),
#' antigen_isos = c("HlyE_IgG", "HlyE_IgA")
#' )
#'
#' summary(est1)
#'
est.incidence <- function(
    pop_data,
    curve_params,
    noise_params,
    antigen_isos = pop_data$antigen_iso |> unique(),
    lambda_start = 0.1,
    stepmin = 1e-8,
    stepmax = 3,
    verbose = FALSE,
    build_graph = FALSE,
    print_graph = build_graph & verbose,
    ...)
{

  if(verbose > 1)
  {
    message('inputs to `est.incidence()`:')
    print(environment() |> as.list())
  }

  .errorCheck(
    data = pop_data,
    antigen_isos = antigen_isos,
    curve_params = curve_params)

  pop_data = pop_data |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select("value", "age", "antigen_iso") |>
    tidyr::drop_na()

  curve_params = curve_params |>
    ungroup() %>%
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1) |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select("y1", "alpha", "d", "antigen_iso") |>
    droplevels()

  noise_params = noise_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    droplevels()

  # incidence can not be calculated if there are zero observations.
  if (nrow(pop_data) == 0) {
    stop("No data provided.")
  }

  if(verbose)
  {
    message("nrow(curve_params) = ", nrow(curve_params))
  }

  if(nrow(noise_params) != length(antigen_isos))
    stop("too many rows of noise parameters.")

  pop_data = pop_data |> split(~antigen_iso)
  curve_params = curve_params |> split(~antigen_iso)
  noise_params = noise_params |> split(~antigen_iso)

  # First, check if we find numeric results...
  res <- .nll(
    pop_data = pop_data,
    log.lambda = log(lambda_start),
    antigen_isos = antigen_isos,
    curve_params = curve_params,
    noise_params = noise_params,
    verbose = verbose,
    ...)

  if (is.na(res)) {
    warning("Could not calculate the log-likelihood with starting parameter value.")
    return(NULL)
  }

  if (verbose)
  {
    message("Initial negative log-likelihood: ", res)
  }

  if (build_graph)
  {
    if(verbose) message('building likelihood graph')
    graph = graph.loglik(
      highlight_points = lambda_start,
      highlight_point_names = "lambda_start",
      pop_data = pop_data,
      antigen_isos = antigen_isos,
      curve_params = curve_params,
      noise_params = noise_params
    )
    if(print_graph)
      print(
        graph +
          ggplot2::scale_x_log10(
            labels = scales::label_comma()))

  } else
  {
    graph = NULL
  }


  if(verbose) message('about to call `nlm()`')
  # Estimate lambda
  time =
    {
      fit = nlm(
        f = .nll,
        p = log(lambda_start),
        pop_data = pop_data,
        antigen_isos = antigen_isos,
        curve_params = curve_params,
        noise_params = noise_params,
        hessian = TRUE,
        stepmax = stepmax,
        steptol = stepmin,
        verbose = verbose,
        print.level = ifelse(verbose, 2, 0),
        ...)
    } |>
    system.time()

  code_text = nlm_exit_codes[fit$code]
  message1 = '\n`nlm()` completed with the following convergence code:\n'
  if(fit$code %in% 3:5)
  {
    warning(
      "`nlm()` may not have reached the maximum likelihood estimate.",
      message1,
      code_text)

  }

  if(verbose)
  {
    message('\nElapsed time: ')
    print(time)
  }

  if(build_graph)
  {
    graph =
      graph |>
      add_point_to_graph(
        fit = fit,
        pop_data = pop_data,
        antigen_isos = antigen_isos,
        curve_params = curve_params,
        noise_params = noise_params)

    if(print_graph)
    {
      print(
        graph +
          ggplot2::scale_x_log10(
            labels = scales::label_comma()))

    }

  }

  fit = fit |>
    structure(
      class = union("seroincidence", class(fit)),
      lambda_start = lambda_start,
      antigen_isos = antigen_isos,
      ll_graph = graph)

  return(fit)
}
