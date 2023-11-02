#' Estimate Seroincidence
#'
#' Function to estimate seroincidences based on cross-section serology data and longitudinal
#' response model.
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional
#'   columns to identify possible `strata`.
#' @param antibodies Character vector with one or more antibody names. Values must match `data`.
#' @param strata Character vector of stratum-defining variables. Values must match with `data`. Default = "".
#' @param numCores Number of processor cores to use for calculations when computing by strata. If
#'   set to more than 1 and package \pkg{parallel} is available, then the computations are
#'   executed in parallel. Default = 1L.
#' @inheritParams .optNll
#' @inheritDotParams .optNll
#'
#' @return A set of lambda estimates for each strata.
#'
#' @examples
#'
#' \dontrun{
#' estimateSeroincidence(data = csData,
#'                       antibodies = c("IgG", "IgM", "IgA"),
#'                       strata = "",
#'                       params = campylobacterDelftParams4,
#'                       censorLimits = cutOffs,
#'                       par0 = baseLn,
#'                       start = -4)
#'
#' estimateSeroincidence(data = csData,
#'                       antibodies = c("IgG", "IgM", "IgA"),
#'                       strata = "",
#'                       params = campylobacterDelftParams4,
#'                       censorLimits = cutOffs,
#'                       par0 = baseLn,
#'                       start = -4,
#'                       numCores = parallel::detectCores())
#' }
#'
#' @export
estimateSeroincidence <- function(
    data,
    antibodies,
    strata = "",
    lnparams,
    noise_params,
    numCores = 1L,
    ...)
{
  if (!"Age" %in% names(data)) {
    data$Age <- rep(NA, nrow(data))
  }

  .errorCheck(
    data = data,
    antibodies = antibodies,
    strata = strata,
    params = lnparams)

  # Split data per stratum
  stratumDataList <- .prepData(
    data = data,
    antibodies = antibodies,
    lnparams = lnparams,
    noise_params = noise_params,
    strata = strata)

  # Loop over data per stratum
  if (numCores > 1L && requireNamespace("parallel", quietly = TRUE)) {
    libPaths <- .libPaths()
    cl <-
      numCores |>
      min(parallel::detectCores()) |>
      parallel::makeCluster()
    on.exit({
      parallel::stopCluster(cl)
    })

    parallel::clusterExport(cl, c("libPaths"), envir = environment())
    parallel::clusterEvalQ(cl, {
      .libPaths(libPaths)
      library(serocalculator)
    })
    fits <- parallel::parLapplyLB(
      cl = cl,
      X = stratumDataList,
      FUN = .optNll,
      ...)
  } else
  {
    fits <- lapply(
      stratumDataList,
      .optNll,
      ...)
  }

  incidenceData <- structure(
    fits,
    Antibodies = antibodies,
    Strata = strata,
    CensorLimits = censorLimits,
    class = c("seroincidenceList", class(fits)))

  return(incidenceData)
}
