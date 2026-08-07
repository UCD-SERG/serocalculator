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
#'
#' library(dplyr)
#'
#' antibodies <- c("HlyE_IgA", "HlyE_IgG")
#'
#' curve <-
#'   typhoid_curves_nostrat_100 |>
#'   filter(antigen_iso %in% antibodies)
#'
#' # Examples must not write into the user's working directory, so send the
#' # cache to a temporary one.
#' cache_dir <- file.path(tempdir(), "serocalculator-example-cache")
#'
#' ests <- est_seroincidence_by_cached(
#'   pop_data = sees_pop_data_pk_100,
#'   sr_params = curve,
#'   noise_params = example_noise_params_pk,
#'   strata = "catchment",
#'   antigen_isos = antibodies,
#'   build_graph = FALSE,
#'   num_cores = 1,
#'   iterlim = 5, # limit iterations for the purpose of this example
#'   cache_path = cache_dir
#' )
#'
#' summary(ests)
est_seroincidence_by_cached <- function(
  ...,
  cache_path = "cache/",
  cache_id = "est_seroincidence_by",
  cache_rerun = FALSE,
  cache_verbose = TRUE,
  cache_extra = NULL
) {
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
