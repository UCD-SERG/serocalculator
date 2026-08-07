#' Simulate multiple data sets, caching the result on disk
#'
#' A caching wrapper around [sim_pop_data_multi()], which dominates the runtime
#' of most simulation studies. The first call simulates and saves; later calls
#' with the same arguments load the saved result instead of re-simulating.
#'
#' The cache is keyed on the **arguments**, so changing any of them (a new
#' `lambdas` vector, a different `nclus`) recomputes automatically, and
#' restoring the old arguments reuses the old result. Argument order and
#' formatting do not matter.
#'
#' The cache is *not* keyed on the implementation. If `sim_pop_data_multi()`
#' or the functions it calls change, an existing cache stays valid as far as
#' this wrapper is concerned. Pass `cache_rerun = TRUE` once to refresh it, or
#' fold a version into the key with, for example,
#' `cache_extra = utils::packageVersion("serocalculator")`. That is
#' deliberately not the default: a development version bumps often, which
#' would discard the cache on nearly every commit.
#'
#' @inheritParams .cache_call
#' @inheritDotParams sim_pop_data_multi
#'
#' @returns a [tibble::tibble()], as returned by [sim_pop_data_multi()].
#' @export
#' @seealso [est_seroincidence_by_cached()]
#'
#' @examples
#' \dontrun{
#' sim_df <- sim_pop_data_multi_cached(
#'   curve_params = typhoid_curves_nostrat_100,
#'   lambdas = c(0.05, 0.1),
#'   nclus = 2,
#'   sample_sizes = 50,
#'   cache_path = "cache/"
#' )
#' }
sim_pop_data_multi_cached <- function(
    ...,
    cache_path = "cache/",
    cache_id = "sim_pop_data_multi",
    cache_rerun = FALSE,
    cache_verbose = TRUE,
    cache_extra = NULL) {
  .cache_call(
    fun = sim_pop_data_multi,
    args = list(...),
    cache_path = cache_path,
    cache_id = cache_id,
    cache_rerun = cache_rerun,
    cache_verbose = cache_verbose,
    cache_extra = cache_extra,
    expected_class = "data.frame"
  )
}
