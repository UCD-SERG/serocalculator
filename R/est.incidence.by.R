#' Estimate Seroincidence
#'
#' Function to estimate seroincidences based on cross-section serology data and longitudinal
#' response model.
#'
#' @param data Data frame with cross-sectional serology data per antibody and age, and additional columns to identify possible `strata`.
#' @param strata Character vector of stratum-defining variables. Values must be variable names in `data`. Default = "".
#' @param strata_curves A subset of `strata`. Values must be variable names in `curve_params`. Default = "".
#' @param strata_noise A subset of `strata`. Values must be variable names in `noise_params`. Default = "".
#' @param numCores Number of processor cores to use for calculations when computing by strata. If set to more than 1 and package \pkg{parallel} is available, then the computations are executed in parallel. Default = 1L.
#' @inheritParams .optNll
#' @inheritDotParams .optNll
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
    strata_curves = strata,
    strata_noise = strata,
    numCores = 1L,
    antigen_isos = data |> pull("antigen_iso") |> unique(),
    ...)
{

  .errorCheck(
    data = data,
    antibodies = antigen_isos,
    strata = strata,
    params = curve_params)

  curve_params =
    curve_params |>
    filter(.data$antigen_iso %in% antigen_isos) |>
    mutate(
      alpha = .data$alpha * 365.25,
      d = .data$r - 1)
  # %>%
  #   select(y1, alpha, d, antigen_iso, any_of(strata))

  # Split data per stratum
  stratumDataList <- prep_data(
    data = data,
    antibodies = antigen_isos,
    curve_params = curve_params,
    noise_params = noise_params,
    strata_varnames = strata,
    strata_curves = strata_curves,
    strata_noise = strata_noise)

  # Loop over data per stratum
  if (numCores > 1L && requireNamespace("parallel", quietly = TRUE)) {
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
      library(serocalculator)
      library(dplyr)
    })
    fits <- parallel::parLapplyLB(
      cl = cl,
      X = stratumDataList,
      fun = function(x) .optNll(dataList = x, ...))
  } else
  {
    fits <- lapply(
      X = stratumDataList,
      FUN = function(x) .optNll(dataList = x, ...))
  }

  incidenceData <- structure(
    fits,
    Antibodies = antigen_isos,
    Strata = stratumDataList |> attr("strata"),
    class = "seroincidence.ests" |> union(class(fits)))

  return(incidenceData)
}
