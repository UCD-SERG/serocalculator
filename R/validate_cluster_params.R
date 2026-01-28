#' Validate cluster and stratum parameters
#'
#' Internal function to validate cluster_var and stratum_var parameters
#' for clustered sampling designs.
#'
#' @param pop_data a [data.frame] with cross-sectional serology data
#' @param cluster_var optional name(s) of cluster identifier variable(s)
#' @param stratum_var optional name of stratum identifier variable
#' @param sampling_weights optional [data.frame] containing sampling weights
#'
#' @return NULL (called for side effects - validation errors)
#' @keywords internal
#' @noRd
.validate_cluster_params <- function(pop_data,
                                     cluster_var = NULL,
                                     stratum_var = NULL,
                                     sampling_weights = NULL) {
  # Validate sampling_weights parameter
  if (!is.null(sampling_weights)) {
    cli::cli_warn(
      "{.arg sampling_weights} is not yet implemented and will be ignored."
    )
  }

  # Validate cluster_var parameter
  if (!is.null(cluster_var)) {
    # Check all cluster variables exist in pop_data
    missing_vars <- setdiff(cluster_var, names(pop_data))
    if (length(missing_vars) > 0) {
      cli::cli_abort(c(
        "x" = paste(
          "{.arg cluster_var} = {.val {missing_vars}}",
          "is not a column in {.arg pop_data}."
        )
      ))
    }
  }

  # Validate stratum_var parameter
  if (!is.null(stratum_var)) {
    if (!stratum_var %in% names(pop_data)) {
      cli::cli_abort(c(
        "x" = paste(
          "{.arg stratum_var} = {.val {stratum_var}}",
          "is not a column in {.arg pop_data}."
        )
      ))
    }
  }

  invisible(NULL)
}
