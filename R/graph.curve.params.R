#' Graph estimated antibody decay curve
#'
#' @param curve_params
#' a [data.frame()] containing MCMC samples of antibody decay curve parameters
#' @param verbose verbose output
#' @param show_all_curves whether to show individual curves under quantiles
#' @param antigen_isos antigen isotypes
#' @param alpha_samples `alpha` parameter passed to [ggplot2::geom_line]
#' (has no effect if `show_all_curves = FALSE`)
#' @param quantiles Optional numeric vector of quantiles to plot
#' (e.g., `c(0.1, 0.5, 0.9)`). If `NULL`, no quantile lines are shown.
#'
#' @returns a [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' curve <-
#'   typhoid_curves_nostrat_100 |>
#'   dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' plot1 <- graph.curve.params(curve)
#'
#' print(plot1)
#'
#' plot2 <- graph.curve.params(curve, show_all_curves = TRUE,
#' quantiles = c(0.1, 0.5, 0.9))
#' print(plot2)
#'
graph.curve.params <- function(
  curve_params,
  antigen_isos = unique(curve_params$antigen_iso),
  verbose = FALSE,
  show_all_curves = FALSE,
  alpha_samples = 0.3,
  quantiles = c(0.1, 0.5, 0.9)  # numeric, flexible
) {
  if (verbose) {
    message("Graphing curves for antigen isotypes: ",
            paste(antigen_isos, collapse = ", "))
  }

  curve_params <- curve_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  tx2 <- 10^seq(-1, 3.1, 0.025)

  bt <- function(y0, y1, t1) {
    log(y1 / y0) / t1
  }

  # uses r > 1 scale for shape
  ab <- function(t, y0, y1, t1, alpha, shape) { # more concise and idiomatic
    beta <- bt(y0, y1, t1)
    if (t <= t1) {
      y0 * exp(beta * t)
    } else {
      (y1^(1 - shape) - (1 - shape) * alpha * (t - t1))^(1 / (1 - shape))
    }
  }

  d <- curve_params  # nolint: object_name_linter

  # FIXED: avoid use of dot in slice() context
  dT_base <- data.frame(t = tx2) |>  # nolint: object_name_linter
    dplyr::mutate(ID = dplyr::row_number()) |>
    tidyr::pivot_wider(names_from = "ID",
                       values_from = "t",
                       names_prefix = "time")
  dT <- dT_base |>
    dplyr::slice(rep(seq_len(nrow(dT_base)), each = nrow(d)))

  serocourse_all <-
    cbind(d, dT) |>
    tidyr::pivot_longer(
      cols       = dplyr::starts_with("time"),
      values_to  = "t"
    ) |>
    dplyr::select(-.data$name) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      res = ab(
        .data$t,
        .data$y0,
        .data$y1,
        .data$t1,
        .data$alpha,
        .data$r
      )
    ) |>
    dplyr::ungroup()

  if (verbose) message("starting to compute quantiles")
  if (!is.null(quantiles)) {
    serocourse_sum <- serocourse_all |>
      dplyr::group_by(.data$antigen_iso, .data$t) |>
      dplyr::summarise(
        res_vals = list(.data$res),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        quantiles_df = purrr::map(
          .data$res_vals,
          ~ tibble::tibble(
            quantile = quantiles,
            res = stats::quantile(.x, probs = quantiles, na.rm = TRUE)
          )
        )
      ) |>
      tidyr::unnest(.data$quantiles_df)
  }

  range <-
    serocourse_all |>
    dplyr::summarize(
      min = min(.data$res, na.rm = TRUE),
      max = max(.data$res, na.rm = TRUE)
    )

  plot1 <- ggplot2::ggplot() +
    ggplot2::aes(x = .data$t, y = .data$res) +
    ggplot2::facet_wrap(~ antigen_iso, ncol = 2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line = ggplot2::element_line()) +
    ggplot2::labs(
      x   = "Days since fever onset",
      y   = "ELISA units",
      col = if (show_all_curves) "MCMC chain" else NULL
    ) +
    ggplot2::theme(legend.position = "bottom")


  if (show_all_curves) {
    group_vars <- c("iter", "chain") |>
      intersect(names(serocourse_all))

    if (length(group_vars) > 1) {
      serocourse_all <-
        serocourse_all |>
        dplyr::mutate(
          iter = interaction(
            dplyr::across(dplyr::all_of(group_vars))
          )
        )

      plot1 <-
        plot1 +
        ggplot2::geom_line(
          data = serocourse_all,
          alpha = alpha_samples,
          aes(
            color = factor(.data$chain),
            group = .data$iter
          )
        )
    } else {
      plot1 <-
        plot1 +
        ggplot2::geom_line(
          data = serocourse_all,
          alpha = alpha_samples,
          aes(group = .data$iter)
        )
    }
    plot1 <-
      plot1 + ggplot2::expand_limits(y = unlist(range))
  }

  plot1 <- plot1 +
    ggplot2::scale_y_log10(
      limits = unlist(range),
      labels = scales::label_comma(),
      minor_breaks = NULL
    )

  if (!is.null(quantiles)) {
    plot1 <- plot1 +
      ggplot2::geom_line(
        data = serocourse_sum,
        aes(
          color = paste0("q", .data$quantile),
          group = .data$quantile
        ),
        linewidth = 0.75
      )

    if (show_all_curves) {
      plot1 <- plot1 +
        ggplot2::labs(col = "MCMC chain")
    }
  }


  return(plot1)
}
