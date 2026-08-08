#' Re-fit a subset of strata, with log-likelihood graphs attached
#'
#' Companion to [est_seroincidence_by_cached()]. Caching a fit built with
#' `build_graph = TRUE` is impractical over many strata, because a
#' log-likelihood plot is attached to each one; the usual pattern is to cache
#' the whole fit with `build_graph = FALSE` and then re-fit, with graphs, only
#' the handful of strata you actually want to plot.
#'
#' Strata are selected by the columns that define them rather than by index or
#' by label, because [count_strata()] names strata positionally
#' (`paste("Stratum", row_number())`). A subset therefore renumbers them, so
#' labels and positions from the full fit do not carry over.
#'
#' @param pop_data a [data.frame()] of cross-sectional serology data: the same
#'   data the full fit was built from.
#' @param strata_ids a [data.frame()] whose columns are the stratifying
#'   variables and whose rows select the strata to re-fit. Matched against
#'   `pop_data` with [dplyr::semi_join()].
#' @param strata a [character()] of the stratifying variable names. Defaults to
#'   the columns of `strata_ids`.
#' @inheritDotParams est_seroincidence_by
#'
#' @returns a `"seroincidence.by"` object covering only the selected strata,
#'   with graphs attached, or `NULL` if `strata_ids` has no rows.
#' @export
#' @seealso [est_seroincidence_by_cached()]
#'
#' @examples
#' \donttest{
#' ests <- est_seroincidence_by(
#'   pop_data = sees_pop_data_pk_100,
#'   sr_params = typhoid_curves_nostrat_100,
#'   noise_params = example_noise_params_pk,
#'   strata = "catchment",
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   build_graph = FALSE,
#'   iterlim = 5
#' )
#'
#' # Re-fit just the strata whose optimizer flagged a possible problem.
#' problem_ids <- ests |>
#'   summary() |>
#'   tibble::as_tibble() |>
#'   dplyr::filter(.data$nlm.convergence.code > 2) |>
#'   dplyr::select("catchment")
#'
#' refit_strata(
#'   pop_data = sees_pop_data_pk_100,
#'   strata_ids = problem_ids,
#'   sr_params = typhoid_curves_nostrat_100,
#'   noise_params = example_noise_params_pk,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   iterlim = 5
#' )
#' }
refit_strata <- function(
  pop_data,
  strata_ids,
  strata = names(strata_ids),
  ...
) {
  if (nrow(strata_ids) == 0) {
    return(NULL)
  }

  selected <-
    pop_data |>
    dplyr::semi_join(strata_ids, by = names(strata_ids))

  return(
    est_seroincidence_by(
      pop_data = selected,
      strata = strata,
      build_graph = TRUE,
      ...
    )
  )
}
