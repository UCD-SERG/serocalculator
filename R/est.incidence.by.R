#' Estimate Seroincidence
#' @description
#' Function to estimate seroincidences based on cross-sectional
#' serology data and longitudinal
#' response model.
#'
#' @param pop_data a [data.frame] with cross-sectional serology data per
#' antibody and age, and additional columns corresponding to
#' each element of the `strata` input
#' @param strata a [character] vector of stratum-defining variables.
#' Values must be variable names in `pop_data`.
#' @param curve_strata_varnames A subset of `strata`.
#' Values must be variable names in `curve_params`. Default = "".
#' @param noise_strata_varnames A subset of `strata`.
#' Values must be variable names in `noise_params`. Default = "".
#' @param num_cores Number of processor cores to use for
#' calculations when computing by strata. If set to
#' more than 1 and package \pkg{parallel} is available,
#' then the computations are executed in parallel. Default = 1L.

#' @details
#'
#' If `strata` is left empty, a warning will be produced,
#' recommending that you use [est.incidence()] for unstratified analyses,
#' and then the data will be passed to [est.incidence()].
#' If for some reason you want to use [est.incidence.by()]
#' with no strata instead of calling [est.incidence()],
#' you may use `NA`, `NULL`, or `""` as the `strata`
#' argument to avoid that warning.
#'
#'
#' @inheritParams est.incidence
#' @inheritDotParams est.incidence
#' @inheritDotParams stats::nlm -f -p -hessian -print.level -steptol
#'
#' @return
#' * if `strata` has meaningful inputs:
#' An object of class `"seroincidence.by"`; i.e., a list of `
#' "seroincidence"` objects from [est.incidence()], one for each stratum,
#' with some meta-data attributes.
#' * if `strata` is missing, `NULL`, `NA`, or `""`:
#' An object of class `"seroincidence"`.
#'
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#' curve <- load_curve_params("https://osf.io/download/rtw5k/") %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) %>%
#'  # Reduce dataset for the purposes of this example
#'   slice(1:100, .by = antigen_iso)
#'
#' noise <- load_noise_params("https://osf.io/download//hqy4v/")
#'
#' est2 <- est.incidence.by(
#'   strata = c("catchment"),
#'   pop_data = xs_data %>% filter(Country == "Pakistan"),
#'   curve_params = curve,
#'   noise_params = noise %>% filter(Country == "Pakistan"),
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   # num_cores = 8 # Allow for parallel processing to decrease run time
#'   iterlim = 5 # limit iterations for the purpose of this example
#' )
#'
#' summary(est2)
#'
est.incidence.by <- function(
    pop_data,
    curve_params,
    noise_params,
    strata,
    curve_strata_varnames = strata,
    noise_strata_varnames = strata,
    antigen_isos = pop_data %>%
      pull("antigen_iso") %>%
      unique(),
    lambda_start = 0.1,
    build_graph = FALSE,
    num_cores = 1L,
    verbose = FALSE,
    print_graph = FALSE,
    ...) {

  strata_is_empty <-
    missing(strata) ||
    is.null(strata) ||
    setequal(strata, NA) ||
    setequal(strata, "")

  if (strata_is_empty) {
    cli::cli_warn(
      class = "strata_empty",
      c(
        "The {.arg strata} argument to {.fn est.incidence.by} is missing.",
        "i" = "If you do not want to stratify your data,
               consider using the {.fn est.incidence} function to
               simplify your code and avoid this warning.",
        "i" = "Since the {.arg strata} argument is empty,
               {.fn est.incidence.by} will return a {.cls seroincidence} object,
               instead of a {.cls seroincidence.by} object."
      )
    )

    to_return <-
      est.incidence(
        pop_data = pop_data,
        curve_params = curve_params,
        noise_params = noise_params,
        lambda_start = lambda_start,
        antigen_isos = antigen_isos,
        build_graph = build_graph,
        verbose = verbose,
        ...
      )
    return(to_return)
  }

  check_strata(pop_data, strata = strata)

  .errorCheck(
    data = pop_data,
    antigen_isos = antigen_isos,
    curve_params = curve_params
  )

  # Split data per stratum
  stratumDataList <- stratify_data(
    antigen_isos = antigen_isos,
    data = pop_data %>% filter(.data$antigen_iso %in% antigen_isos),
    curve_params = curve_params %>% filter(.data$antigen_iso %in% antigen_isos),
    noise_params = noise_params %>% filter(.data$antigen_iso %in% antigen_isos),
    strata_varnames = strata,
    curve_strata_varnames = curve_strata_varnames,
    noise_strata_varnames = noise_strata_varnames
  )

  strata_table <- stratumDataList %>% attr("strata")

  if (verbose) {
    cli::cli_inform("Data has been stratified.")
    cli::cli_inform("Here are the strata that will be analyzed:")
    print(strata_table)
  }

  if (num_cores > 1L && !requireNamespace("parallel", quietly = TRUE)) {
    cli::cli_warn(
      "The `parallel` package is not installed,
      so `num_cores > 1` has no effect.",
      "To install `parallel`, run `install.packages('parallel')`
      in the console."
    )
  }

  # Loop over data per stratum
  if (num_cores > 1L) {
    requireNamespace("parallel", quietly = FALSE)

    num_cores <- num_cores %>% check_parallel_cores()

    if (verbose) {
      message("Setting up parallel processing with
              `num_cores` = ", num_cores, ".")
    }

    libPaths <- .libPaths()
    cl <-
      num_cores %>%
      parallel::makeCluster() %>%
      suppressMessages()
    on.exit({
      parallel::stopCluster(cl)
    })

    # Export library paths to the cluster
    parallel::clusterExport(cl, c("libPaths"), envir = environment())

    # Evaluate library loading on the cluster
    parallel::clusterEvalQ(cl, {
      .libPaths(libPaths)
      # note - this gets out of sync when using load_all() in development
      require(serocalculator)
      require(dplyr)
    })

    # Perform parallel computation and record execution time
    time <- system.time({
      fits <- parallel::parLapplyLB(
        cl = cl,
        X = stratumDataList,
        fun = function(x) {
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
                ...
              )
            )
          )
        }
      )
    })

    if (verbose) {
      message("Elapsed time for parallelized code: ")
      print(time)
    }
  } else {
    fits <- list()  # Initialize an empty list for fits

    # Time progress
    for (cur_stratum in names(stratumDataList)) {
      cur_stratum_vars <- strata_table %>%
        dplyr::filter(.data$Stratum == cur_stratum)

      if (verbose) {
        message("starting new stratum: ", cur_stratum)
        print(cur_stratum_vars)
      }

      fits[[cur_stratum]] <- do.call(
        what = est.incidence,
        args = c(
          stratumDataList[[cur_stratum]],
          list(
            lambda_start = lambda_start,
            antigen_isos = antigen_isos,
            build_graph = build_graph,
            print_graph = print_graph,
            verbose = verbose,
            ...
          )
        )
      )
    }


    if (verbose) {
      message("Elapsed time for loop over strata: ")
      print(time)
    }
  }

  incidenceData <- structure(
    fits,
    antigen_isos = antigen_isos,
    Strata = strata_table,
    graphs_included = build_graph,
    class = "seroincidence.by" %>% union(class(fits))
  )

  return(incidenceData)
}
