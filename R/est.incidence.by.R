#' Estimate Seroincidence
#'
#' Function to estimate seroincidences based on cross-section serology data and longitudinal
#' response model.
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns to identify possible `strata`.
#' @param strata Character vector of stratum-defining variables. Values must be variable names in `data`. Default = "".
#' @param curve_strata_varnames A subset of `strata`. Values must be variable names in `curve_params`. Default = "".
#' @param noise_strata_varnames A subset of `strata`. Values must be variable names in `noise_params`. Default = "".
#' @param numCores Number of processor cores to use for calculations when computing by strata. If set to more than 1 and package \pkg{parallel} is available, then the computations are executed in parallel. Default = 1L.

#' @inheritParams .optNll
#' @inheritDotParams .optNll -dataList
#' @inheritDotParams stats::nlm -f -p -hessian
#'
#' @return A set of lambda estimates for each strata.
#'
#'
#' @export
est.incidence.by <- function(
    data,
    curve_params,
    noise_params,
    strata = "",
    curve_strata_varnames = strata,
    noise_strata_varnames = strata,
    antigen_isos = data |> pull("antigen_iso") |> unique(),
    lambda.start = 1/365.25,
    numCores = 1L,
    verbose = FALSE,
    ...)
{

  .errorCheck(
    data = data,
    antibodies = antigen_isos,
    strata = strata,
    params = curve_params)

  curve_params =
    curve_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos) |>
    dplyr::mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1)

  noise_params =
    noise_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)
  # %>%
  #   select(y1, alpha, d, antigen_iso, any_of(strata))

  # Split data per stratum
  stratumDataList <- prep_data(
    data = data,
    antibodies = antigen_isos,
    curve_params = curve_params,
    noise_params = noise_params,
    strata_varnames = strata,
    curve_strata_varnames = curve_strata_varnames,
    noise_strata_varnames = noise_strata_varnames)

  if(verbose) message("Data has been stratified.")

  # Loop over data per stratum
  if (numCores > 1L && requireNamespace("parallel", quietly = TRUE)) {

    if(verbose) message("Setting up parallel processing.")

    libPaths <- .libPaths()
    cl <-
      numCores |>
      min(parallel::detectCores() - 1) |>
      parallel::makeCluster()
    on.exit({
      parallel::stopCluster(cl)
    })

    parallel::clusterExport(cl, c("libPaths"), envir = environment())
    parallel::clusterEvalQ(cl, {
      .libPaths(libPaths)
      require(serocalculator) # note - this gets out of sync when using load_all() in development
      require(dplyr)

    })

    {
      fits <- parallel::parLapplyLB(
        cl = cl,
        X = stratumDataList,
        fun = function(x)
          .optNll(
            dataList = x,
            lambda.start = lambda.start,
            antigen_isos = antigen_isos,
            ...)
      )
    } |> system.time() -> time

    if(verbose)
    {
      message("Elapsed time for parallelized code: ")
      print(time)
    }
  } else
  {
    # fits <- lapply(
    #   X = stratumDataList,
    #   FUN = function(x) .optNll(dataList = x, verbose = verbose, ...))

    fits = list()

    { # time progress

      for (cur_stratum in names(stratumDataList))
      {
        cur_stratum_vars =
          stratumDataList |>
          attr("strata") |>
          dplyr::filter(.data$Stratum == cur_stratum)

        stratum_string =
          paste(
            names(cur_stratum_vars),
            cur_stratum_vars,
            sep = ": ") |>
          paste(collapse = ", ")

        if(verbose)
        {
          message('starting new stratum: ', cur_stratum)
          print(cur_stratum_vars)
        }

        fits[[cur_stratum]] =
          .optNll(
            lambda.start = lambda.start,
            dataList = stratumDataList[[cur_stratum]],
            antigen_isos = antigen_isos,
            verbose =  verbose,
            ...) |>
          structure(stratum_string = stratum_string)

      }
    } |> system.time() -> time

    if(verbose)
    {
      message("Elapsed time for loop over strata: ")
      print(time)
    }
  }

  incidenceData <- structure(
    fits,
    Antibodies = antigen_isos,
    Strata = stratumDataList |> attr("strata"),
    class = "seroincidence.ests" |> union(class(fits)))

  return(incidenceData)
}
