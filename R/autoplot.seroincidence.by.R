#' Plot `seroincidence.by` log-likelihoods
#' @description
#' Plots log-likelihood curves by stratum, for `seroincidence.by` objects
#' @param object a '"seroincidence.by"' object (from [est_seroincidence_by()])
#' @param ncol number of columns to use for panel of plots
#' @inheritDotParams autoplot.seroincidence
#' @return a `"ggarrange"` object: a single or [list()] of [ggplot2::ggplot()]s
#' @export
#' @examples
#'\donttest{
#' library(dplyr)
#' library(ggplot2)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' est2 <- est_seroincidence_by(
#'   strata = c("catchment"),
#'   pop_data = xs_data,
#'   sr_params = curve,
#'   curve_strata_varnames= NULL,
#'   noise_strata_varnames = NULL,
#'   noise_params = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   #num_cores = 8, #Allow for parallel processing to decrease run time
#'   build_graph = TRUE
#' )
#'
#' # Plot the log-likelihood curve
#' autoplot(est2)
#'}
autoplot.seroincidence.by <- function(
    object,
    ncol = min(3, length(object)),
    ...) {
  if (length(object) == 0) {
    stop("The input doesn't contain any fits. Did subsetting go wrong?")
  }

  if (!attr(object, "graphs_included")) {
    stop(
      "Graphs cannot be extracted; ",
      "`build_graph` was not `TRUE` in the call to `est_seroincidence_by()`"
    )
    figure <- NULL
  }

  labels <- names(object)
  figs <- lapply(object, FUN = autoplot.seroincidence, ...)

  for (i in seq_along(figs)){
    figs[[i]] <- figs[[i]] + ggplot2::ggtitle(labels[i])
  }


  nrow <- ceiling(length(figs) / ncol)
  figure <- do.call(
    what = function(...) {
      ggpubr::ggarrange(
        ...,
        ncol = ncol,
        nrow = nrow
      )
    },
    args = figs
  )

  return(figure)
}
