#' Validate and normalize verbose argument
#'
#' @description
#' Internal helper to validate and normalize verbose argument to a numeric
#' level. Accepts logical or non-negative whole number inputs and converts to
#' a standardized numeric verbosity level.
#'
#' @param verbose A logical scalar or non-negative whole number specifying
#' verbosity level.
#'
#' @return A numeric scalar verbosity level (0, 1, 2, etc.)
#'
#' @details
#' No upper bound is enforced on numeric verbosity values. Since `verbose` is
#' used only in `>=1`/`>=2` comparisons, any valid non-negative whole number
#' produces correct behavior. An upper-bound check based on
#' `.Machine$integer.max` would reject values that behave perfectly fine and
#' would introduce an undocumented constraint that cannot be validated against
#' the documented behavior.
#'
#' @keywords internal
#' @noRd
.validate_verbose <- function(verbose) {
  is_valid_numeric_verbose <- is.numeric(verbose) &&
    length(verbose) == 1 &&
    !is.na(verbose) &&
    is.finite(verbose) &&
    verbose >= 0 &&
    verbose == floor(verbose)

  if (is.logical(verbose) && length(verbose) == 1 && !is.na(verbose)) {
    # Coerce logical verbosity to documented level semantics:
    # FALSE -> 0 and TRUE -> 1.
    verbose_level <- as.numeric(verbose)
  } else if (is_valid_numeric_verbose) {
    verbose_level <- verbose
  } else {
    cli::cli_abort(c(
      "{.arg verbose} must be a single logical or non-negative whole number.",
      "i" = paste(
        "Use `FALSE`/`0` for no messages, `TRUE`/`1` for",
        "basic messages, and `2` or larger integers for",
        "detailed input logging."
      )
    ))
  }

  return(verbose_level)
}
