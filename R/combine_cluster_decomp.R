#' Combine one-way subset variances into a cluster-robust variance
#'
#' @description
#' Assembles the multi-way cluster-robust variance from its one-way subset
#' terms using the Cameron, Gelbach, and Miller (2011) inclusion-exclusion
#' formula, floors a negative raw estimate at 0, and optionally floors the
#' result at the model-based variance.
#'
#' Negative estimates are a documented property of the multi-way estimator, so
#' the raw sum is floored at 0 (with a warning) regardless of
#' `floor_to_standard`; "negative variance" is never a valid result. Flooring
#' at the model-based variance is a separate, optional choice. If any subset
#' variance is unavailable (e.g. a degenerate Hessian returned `NA`), the raw
#' sum is non-finite; this is caught and reported rather than allowed to
#' propagate to a silent `NaN` standard error.
#'
#' A degenerate Hessian poisons `standard_var` (`1 / hessian`) exactly as it
#' poisons the one-way subset terms, since both are computed from the same
#' fitted model. So the `floor_to_standard` fallback only uses `standard_var`
#' when it is itself finite and positive; otherwise it also returns a missing
#' standard error, rather than silently falling back to a negative or
#' infinite "variance".
#'
#' @param decomp_terms a [tibble::tibble()] of signed subset variance terms,
#'   with at least a `signed_term` column
#' @param standard_var model-based variance of log(lambda)
#' @param floor_to_standard whether to floor the robust variance at
#'   `standard_var`
#'
#' @return a named [list()] with `standard_var`, `robust_raw`, `robust_final`,
#'   `terms`, and `floor_applied`
#' @keywords internal
#' @noRd
.combine_cluster_decomp <- function(
  decomp_terms,
  standard_var,
  floor_to_standard = FALSE
) {
  robust_raw <- sum(decomp_terms$signed_term)

  if (!is.finite(robust_raw)) {
    # A subset variance was unavailable (e.g. a degenerate Hessian returned
    # NA), which poisons the inclusion-exclusion sum. Handle it here so the
    # non-finite value never reaches sqrt() as a silent NaN standard error --
    # note plain max() would leave max(standard_var, NA) == NA, defeating the
    # floor. Fall back to the model-based variance only when it was
    # requested AND is itself usable: a degenerate Hessian poisons
    # standard_var = 1 / hessian too (negative when hessian < 0, Inf when
    # hessian == 0), so falling back unconditionally would silently trade
    # one invalid "variance" for another.
    standard_var_usable <- is.finite(standard_var) && standard_var > 0
    can_fall_back <- isTRUE(floor_to_standard) && standard_var_usable
    robust_final <- if (can_fall_back) standard_var else NA_real_
    floor_applied <- can_fall_back
    fallback_msg <- if (can_fall_back) {
      "Falling back to the model-based variance ({signif(standard_var, 6)})."
    } else if (isTRUE(floor_to_standard)) {
      "The model-based variance is also unavailable
        ({signif(standard_var, 6)}); returning a missing standard error."
    } else {
      "Returning a missing standard error; set {.arg floor_to_standard} to
        {.code TRUE} to fall back to the model-based variance."
    }
    cli::cli_warn(c(
      "!" = "One or more subset variances were unavailable, so the multi-way
        cluster-robust variance could not be computed.",
      "i" = fallback_msg
    ))
  } else {
    if (robust_raw < 0) {
      cli::cli_warn(c(
        "!" = "Multi-way cluster-robust variance was negative
          ({signif(robust_raw, 6)}); flooring it at 0.",
        "i" = "Negative estimates can arise from the inclusion-exclusion
          combination of one-way cluster variances
          (Cameron, Gelbach, and Miller, 2011)."
      ))
    }
    robust_nonneg <- max(0, robust_raw)
    floor_applied <- isTRUE(floor_to_standard) && robust_nonneg < standard_var
    robust_final <- if (isTRUE(floor_to_standard)) {
      max(standard_var, robust_nonneg)
    } else {
      robust_nonneg
    }
  }

  cluster_decomp <- list(
    standard_var = standard_var,
    robust_raw = robust_raw,
    robust_final = robust_final,
    terms = decomp_terms,
    floor_applied = floor_applied
  )
  cluster_decomp
}
