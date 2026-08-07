#' Estimate seroincidence by stratum, caching the result on disk
#'
#' A caching wrapper around [est_seroincidence_by()]. The first call estimates
#' and saves; later calls with the same arguments load the saved result.
#'
#' The cache is keyed on the arguments, with the same scope and the same
#' caveat about implementation changes described in
#' [sim_pop_data_multi_cached()].
#'
#' Note that `build_graph = TRUE` attaches a log-likelihood plot to every
#' stratum, which makes the returned object far larger on disk than the same
#' fit without graphs. When caching many strata, prefer `build_graph = FALSE`
#' and re-fit the handful of strata you actually plot.
#'
#' @inheritParams .cache_call
#' @inheritDotParams est_seroincidence_by
#'
#' @returns a `"seroincidence.by"` object, as returned by
#'   [est_seroincidence_by()].
#' @export
#' @seealso [sim_pop_data_multi_cached()]
#'
#' @examples
#' \dontrun{
#' ests <- est_seroincidence_by_cached(
#'   pop_data = sim_df,
#'   sr_params = typhoid_curves_nostrat_100,
#'   noise_params = noise_params,
#'   strata = c("lambda.sim", "cluster"),
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   build_graph = FALSE,
#'   cache_path = "cache/"
#' )
#' }
est_seroincidence_by_cached <- function(
    ...,
    cache_path = "cache/",
    cache_id = "est_seroincidence_by",
    cache_rerun = FALSE,
    cache_verbose = TRUE,
    cache_extra = NULL) {
  .cache_call(
    fun = est_seroincidence_by,
    args = list(...),
    cache_path = cache_path,
    cache_id = cache_id,
    cache_rerun = cache_rerun,
    cache_verbose = cache_verbose,
    cache_extra = cache_extra,
    expected_class = "seroincidence.by"
  )
}
