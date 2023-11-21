# .optNll = function(x,...) x[[1]] |> filter(antigen_iso  == "HlyE_IgA") |> head()


#' Find the maximum likelihood estimate of the incidence rate parameter
#'
#' @param lambda.start starting guess for incidence rate, in years/event.
#' @param antigen_isos Character vector with one or more antibody names. Values must match `data`
#' @param dataList Optional argument; as an alternative to passing in `data`, `curve_params`, and `noise_params` individually, you may create a list containing these three elements (with these names) and pass that in instead. This option may be useful for parallel processing across strata.
#' @inheritParams .nll
#' @inheritParams stats::nlm
#' @inheritDotParams stats::nlm -f -p -hessian

#' @returns a [stats::nlm()] fit object
#' @export
.optNll <- function(
    data = dataList$data,
    curve_params = dataList$curve_params,
    noise_params = dataList$noise_params,
    dataList = NULL,
    antigen_isos = data |> pull("antigen_iso") |> unique(),
    lambda.start = 1/365.25,
    stepmax = 1,
    verbose = FALSE,
    build_graph = TRUE,
    ...)
{

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

  if(verbose)
  {
    message("Initial log-likelihood: ", res)
  }

  if(build_graph)
  {
    if(verbose) message('building likelihood graph')
    graph = graph_loglik(
      lambda.start = lambda.start,
      data = data,
      antigen_isos = antigen_isos,
      curve_params = curve_params,
      noise_params = noise_params
    )
  } else
  {
    graph = NULL
  }


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
        verbose = verbose,
        ...)
    } |>
    system.time()

  if(verbose)
  {
    message('elapsed time: ')
    print(time)
  }

  if(build_graph)
  {
    graph =
      graph +
      ggplot2::geom_point(
        data = tibble(
          x = exp(fit$estimate),
          y = -.nll_vec(fit$estimate, ...)),
        aes(x = x, y = y, col = "est.incidence")
      )
  }

  fit = fit |>
    structure(
      class = "seroincidence.est" |> union(class(fit)),
      lambda.start = lambda.start,
      ll_graph = graph)

  return(fit)
}
