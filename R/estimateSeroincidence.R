#' Estimate Seroincidence
#'
#' Function to estimate seroincidences based on cross-section serology data and longitudinal
#' response model.
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional
#'   columns to identify possible `strata`.
#' @param antibodies Character vector with one or more antibody names. Values must match `data`.
#' @param strata Character vector of stratum-defining variables. Values must match with `data`. Default = "".
#' @param params List of data frames of all longitudinal parameters. Each data frame contains
#'   Monte Carlo samples for each antibody type.
#' @param censorLimits List of cutoffs for one or more named antibody types (corresponding to
#'   `data`).
#' @param par0 List of parameters for the (lognormal) distribution of antibody concentrations
#'   for true seronegatives (i.e. those who never seroconverted), by named antibody type
#'   (corresponding to `data`).
#' @param start A starting value for `log(lambda)`. Value of -6 corresponds roughly to 1 day
#'   (log(1/365.25)), -4 corresponds roughly to 1 week (log(7 / 365.25)). Default = -6.
#' @param numCores Number of processor cores to use for calculations when computing by strata. If
#'   set to more than 1 and package \pkg{parallel} is available, then the computations are
#'   executed in parallel. Default = 1L.
#'
#' @return
#' A set of lambda estimates for each strata.
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
    params,
    censorLimits,
    par0,
    start = -6,
    numCores = 1L)
{
  if (!"Age" %in% names(data)) {
    data$Age <- rep(NA, nrow(data))
  }

  .errorCheck(data = data,
              antibodies = antibodies,
              strata = strata,
              params = params)

  antibodiesData <- .prepData(data = data,
                              antibodies = antibodies,
                              strata = strata)

  # Split data per stratum
  stratumDataList <- split(
    antibodiesData$Data,
    antibodiesData$Data$Stratum)

  # Loop over data per stratum
  if (numCores > 1L && requireNamespace("parallel", quietly = TRUE)) {
    libPaths <- .libPaths()
    cl <- parallel::makeCluster(min(numCores, parallel::detectCores()))
    on.exit({
      parallel::stopCluster(cl)
    })

    parallel::clusterExport(cl, c("libPaths"), envir = environment())
    parallel::clusterEvalQ(cl, {
      .libPaths(libPaths)
      library(seroincidence)
    })
    fits <- parallel::parLapplyLB(
      cl,
      stratumDataList,
      .optNll,
      antibodies = antibodies,
      params = params,
      censorLimits = censorLimits,
      ivc = ivc,
      m = 0,
      par0 = par0,
      start = start)
  } else {
    fits <- lapply(
      stratumDataList,
      .optNll,
      antibodies = antibodies,
      params = params,
      censorLimits = censorLimits,
      ivc = ivc,
      m = 0,
      par0 = par0,
      start = start)
  }

  incidenceData <- list(
    Fits = fits,
    Antibodies = antibodies,
    Strata = strata,
    CensorLimits = censorLimits)

  class(incidenceData) <- c("seroincidence", "list")

  return(incidenceData)
}
