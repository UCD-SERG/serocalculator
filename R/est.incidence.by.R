#' Estimate Seroincidence
#'
#' @description
#' Function to estimate seroincidences based on cross-section serology data and longitudinal
#' response model.
#'
#' @param pop_data [data.frame()] with cross-sectional serology data per antibody and age, and additional columns to identify possible `strata`.
#' @param strata Character vector of stratum-defining variables. Values must be variable names in `pop_data`.
#' @param curve_strata_varnames A subset of `strata`. Values must be variable names in `curve_params`. Default = "".
#' @param noise_strata_varnames A subset of `strata`. Values must be variable names in `noise_params`. Default = "".
#' @param num_cores Number of processor cores to use for calculations when computing by strata. If set to more than 1 and package \pkg{parallel} is available, then the computations are executed in parallel. Default = 1L.

#' @details
#'
#' If `strata` is left empty, a warning will be produced, recommending that you use `est.incidence()` for unstratified analyses, and then the data will be passed to `est.incidence()`. If for some reason you want to use `est.incidence.by()` with no strata instead of calling `est.incidence()`, you may use `NA`, `NULL`, or "" as the `strata` argument to avoid that warning.
#'
#'
#' @inheritParams est.incidence
#' @inheritDotParams est.incidence
#' @inheritDotParams stats::nlm -f -p -hessian -print.level -steptol
#'
#' @return
#' * if `strata` has meaningful inputs:
#' An object of class `"seroincidence.by"`; i.e., a list of `"seroincidence"` objects from [est.incidence()], one for each stratum, with some meta-data attributes.
#' * if `strata` is missing, `NULL`, `NA`, or `""`:
#' An object of class `"seroincidence"`.
#'
#' @export
#' @examples
#' xs_data = load_pop_data("https://osf.io/download//n6cp3/")
#' curve = load_curve_params("https://osf.io/download/rtw5k/")
#' noise = load_noise_params("https://osf.io/download//hqy4v/")
#'
#' est2 = est.incidence.by(
#' strata = c("catchment"),
#' pop_data = xs_data %>% filter(Country == "Pakistan"),
#' curve_params = curve,
#' noise_params = noise %>% filter(Country == "Pakistan"),
#' antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#' num_cores = 8 #Allow for parallel processing to decrease run time
#' )
#'
#' summary(est2)
est.incidence.by <- function(
    pop_data,
    curve_params,
    noise_params,
    strata,
    curve_strata_varnames = strata,
    noise_strata_varnames = strata,
    antigen_isos = pop_data |> pull("antigen_iso") |> unique(),
    lambda_start = 0.1,
    build_graph = FALSE,
    num_cores = 1L,
    verbose = FALSE,
    print_graph = FALSE,
    ...)
{

  if(missing(strata))
  {
    warning(
      "The `strata` argument to `est.incidence.by()` is missing.",
      "\n\n  If you do not want to stratify your data, ",
      "consider using the `est.incidence()` function to simplify your code and avoid this warning.",
      "\n\n Since the `strata` argument is empty, `est.incidence.by()` will return a `seroincidence` object, instead of a `seroincidence.by` object.\n")
  }

  strata_is_empty =
    missing(strata) ||
    is.null(strata) ||
    setequal(strata, NA) ||
    setequal(strata, "")

  if(strata_is_empty)
  {
    to_return =
      est.incidence(
        pop_data = pop_data,
        curve_params = curve_params,
        noise_params = noise_params,
        lambda_start = lambda_start,
        antigen_isos = antigen_isos,
        build_graph = build_graph,
        verbose = verbose,
        ...)
    return(to_return)
  }

  .checkStrata(data = pop_data, strata = strata)

  .errorCheck(
    data = pop_data,
    antigen_isos = antigen_isos,
    curve_params = curve_params)

  # Split data per stratum
  stratumDataList <- stratify_data(
    antigen_isos = antigen_isos,
    data = pop_data |> filter(.data$antigen_iso %in% antigen_isos),
    curve_params = curve_params |> filter(.data$antigen_iso %in% antigen_isos),
    noise_params = noise_params |> filter(.data$antigen_iso %in% antigen_isos),
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

  if(num_cores > 1L && !requireNamespace("parallel", quietly = TRUE))
  {
    warning(
      "The `parallel` package is not installed, so `num_cores > 1` has no effect.",
      "To install `parallel`, run `install.packages('parallel')` in the console.")
  }

  # Loop over data per stratum
  if (num_cores > 1L)
  {
    requireNamespace("parallel", quietly = FALSE)

    num_cores = num_cores |> check_parallel_cores()

    if(verbose)
    {
      message("Setting up parallel processing with `num_cores` = ", num_cores, ".")
    }


    libPaths <- .libPaths()
    cl <-
      num_cores |>
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
          do.call(
            what = est.incidence,
            args = c(
              x,
              list(
                lambda_start = lambda_start,
                antigen_isos = antigen_isos,
                build_graph = build_graph,
                print_graph = FALSE,
                verbose = FALSE,
                ...)
            )
          )
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
          do.call(
            what = est.incidence,
            args = c(
              stratumDataList[[cur_stratum]],
              list(
                lambda_start = lambda_start,
                antigen_isos = antigen_isos,
                build_graph = build_graph,
                print_graph = print_graph,
                verbose = verbose,
                ...)
            )
          )


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
