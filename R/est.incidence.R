#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' This function models seroincidence using maximum likelihood estimation; that is, it finds the value of the seroincidence parameter which maximizes the likelihood (i.e., joint probability) of the data.
#' @inheritParams .nll
#' @inheritParams stats::nlm
#' @param lambda.start starting guess for incidence rate, in years/event.
#' @param antigen_isos Character vector with one or more antibody names. Values must match `data`
#' @param c.age age category to subset data by (optional)
#' @param dataList Optional argument; as an alternative to passing in `data`, `curve_params`, and `noise_params` individually, you may create a list containing these three elements (with these names) and pass that in instead. This option may be useful for parallel processing across strata.
#' @param build_graph whether to graph the log-likelihood function across a range of incidence rates (lambda values)
#' @param print_graph whether to display the log-likelihood curve graph in the course of running `est.incidence()`
#' @param stepmin A positive scalar providing the minimum allowable relative step length.
#' @inheritDotParams stats::nlm -f -p -hessian -print.level -steptol

#' @returns a `"seroincidence"` object, which is a [stats::nlm()] fit object with extra meta-data attributes `lambda.start`, `antigen_isos`, and `ll_graph`
#' @export
est.incidence <- function(
    data = dataList$data,
    curve_params = dataList$curve_params,
    noise_params = dataList$noise_params,
    dataList = NULL,
    antigen_isos = data$antigen_iso |> unique(),
    lambda.start = 0.1,
    stepmin = 1e-8,
    stepmax = 3,
    verbose = FALSE,
    build_graph = TRUE,
    print_graph = build_graph & verbose,
    c.age = NULL,
    ...)
{
  if(!is.null(c.age))
  {
    data = data %>% dplyr::filter(.data[["ageCat"]] == c.age)
    curve_params = curve_params %>% dplyr::filter(.data[["ageCat"]] == c.age)

    if("ageCat" %in% names(noise_params))
    {
      noise_params =
        noise_params %>%
        dplyr::filter(.data[["ageCat"]] == c.age)
    }
  }
  data = data |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select("y", "a", "antigen_iso") |>
    drop_na()

  curve_params = curve_params |>
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1) |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::select("y1", "alpha", "d", "antigen_iso")

  noise_params = noise_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  # incidence can not be calculated if there are zero observations.
  if (nrow(data) == 0) {
    stop("No data provided.")
  }

  if(verbose)
  {
    message("nrow(curve_params) = ", nrow(curve_params))
  }

  if(nrow(noise_params) != length(antigen_isos))
    stop("too many rows of noise parameters.")

  data = data |> split(~antigen_iso)
  curve_params = curve_params |> split(~antigen_iso)
  noise_params = noise_params |> split(~antigen_iso)

  # First, check if we find numeric results...
  res <- .nll(
    data = data,
    log.lambda = log(lambda.start),
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
    message("Initial log-likelihood: ", res)
  }

  if (build_graph)
  {
    if(verbose) message('building likelihood graph')
    graph = graph_loglik(
      lambda.start = lambda.start,
      data = data,
      antigen_isos = antigen_isos,
      curve_params = curve_params,
      noise_params = noise_params
    )
    if(print_graph)
      print(
        graph +
          ggplot2::scale_x_continuous(
            trans = "log10",
            labels = scales::label_comma()))

  } else
  {
    graph = NULL
  }


  if(verbose) message('about to call `nlm()`')
  # Estimate log.lambda
  time =
    {
      fit = nlm(
        f = .nll,
        p = log(lambda.start),
        data = data,
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
  message1 = '\n`nlm()` completed with the following exit code:\n'
  if(fit$code %in% 3:5)
  {
    warning("`nlm()` may not have reached the maximum likelihood estimate.", message1, code_text)

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
        data = data,
        antigen_isos = antigen_isos,
        curve_params = curve_params,
        noise_params = noise_params)

    if(print_graph) print(graph)

  }

  fit = fit |>
    structure(
      class = union("seroincidence", class(fit)),
      lambda.start = lambda.start,
      antigen_isos = antigen_isos,
      ll_graph = graph)

  return(fit)
}
