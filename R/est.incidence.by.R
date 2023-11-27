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

#' @inheritParams est.incidence
#' @inheritDotParams est.incidence -dataList
#' @inheritDotParams stats::nlm -f -p -hessian
#'
#' @return An object of class `"seroincidence.by"`: a list of `"seroincidence` objects from [est.incidence()], one for each stratum, with some meta-data attributes.
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
    build_graph = TRUE,
    numCores = 1L,
    verbose = FALSE,
    ...)
{

  .errorCheck(
    data = data,
    antigen_isos = antigen_isos,
    strata = strata,
    params = curve_params)

  # Split data per stratum
  stratumDataList <- prep_data(
    data = data,
    antigen_isos = antigen_isos,
    curve_params = curve_params,
    noise_params = noise_params,
    strata_varnames = strata,
    curve_strata_varnames = curve_strata_varnames,
    noise_strata_varnames = noise_strata_varnames)

  strata_table = stratumDataList |> attr("strata")

  if(verbose)
  {
    message("Data has been stratified.")
    message('Here are the strata that will be analyzed:')
    print(strata_table)
  }

  if(numCores > 1L && !requireNamespace("parallel", quietly = TRUE))
  {
    warning(
      "The `parallel` package is not installed, so `numCores > 1` has no effect.",
      "To install `parallel`, run `install.packages('parallel')` in the console.")
  }

  # Loop over data per stratum
  if (numCores > 1L)
  {
    if(verbose) message("Setting up parallel processing.")
    requireNamespace("parallel", quietly = FALSE)

    libPaths <- .libPaths()
    cl <-
      numCores |>
      min(parallel::detectCores() - 1) |>
      parallel::makeCluster() |>
      suppressMessages()
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
          est.incidence(
            dataList = x,
            lambda.start = lambda.start,
            antigen_isos = antigen_isos,
            build_graph = build_graph,
            verbose = FALSE,
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
    #   FUN = function(x) est.incidence(dataList = x, verbose = verbose, ...))

    fits = list()

    { # time progress

      for (cur_stratum in names(stratumDataList))
      {
        cur_stratum_vars =
          strata_table |>
          dplyr::filter(.data$Stratum == cur_stratum)

        if(verbose)
        {
          message('starting new stratum: ', cur_stratum)
          print(cur_stratum_vars)
        }

        fits[[cur_stratum]] =
          est.incidence(
            lambda.start = lambda.start,
            dataList = stratumDataList[[cur_stratum]],
            antigen_isos = antigen_isos,
            build_graph = build_graph,
            verbose =  verbose,
            ...)

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
    antigen_isos = antigen_isos,
    Strata = strata_table,
    graphs_included = build_graph,
    class = "seroincidence.by" |> union(class(fits)))

  return(incidenceData)
}
