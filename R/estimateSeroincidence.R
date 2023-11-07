#' Estimate Seroincidence
#'
#' Function to estimate seroincidences based on cross-section serology data and longitudinal
#' response model.
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns to identify possible `strata`.
#' @param strata Character vector of stratum-defining variables. Values must match with `data`. Default = "".
#' @param numCores Number of processor cores to use for calculations when computing by strata. If set to more than 1 and package \pkg{parallel} is available, then the computations are executed in parallel. Default = 1L.
#' @inheritParams .optNll
#' @inheritDotParams .optNll
#'
#' @return A set of lambda estimates for each strata.
#'
#'
#' @export
estimateSeroincidence <- function(
    data,
    lnparams,
    noise_params,
    strata = "",
    numCores = 1L,
    antigen_isos = data |> pull(antigen_iso) |> unique(),
    ...)
{

  if (!"Age" %in% names(data)) {
    data$Age <- rep(NA, nrow(data))
  }

  .errorCheck(
    data = data,
    antibodies = antigen_isos,
    strata = strata,
    params = lnparams)

  lnparams =
    lnparams |>
    filter(antigen_iso %in% antigen_isos) |>
    mutate(
      alpha = alpha * 365.25,
      d = r - 1)
  # %>%
  #   select(y1, alpha, d, antigen_iso, any_of(strata))

  # Split data per stratum
  stratumDataList <- .prepData(
    data = data,
    antibodies = antigen_isos,
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
      FUN = function(x) .optNll(dataList = x, ...))
  } else
  {
    fits <- lapply(
      X = stratumDataList,
      F = function(x) .optNll(dataList = x, ...))
  }

  incidenceData <- structure(
    fits,
    Antibodies = antigen_isos,
    Strata = strata,
    class = c("seroincidenceList", class(fits)))

  return(incidenceData)
}
