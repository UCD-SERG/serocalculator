#' Graph estimated antibody decay curve
#'
#' @param curve_params
#' a [data.frame()] containing MCMC samples of antibody decay curve parameters
#' @param verbose verbose output
#' @param show_all_curves whether to show individual curves under quantiles
#' @param antigen_isos antigen isotypes
#' @param alpha_samples `alpha` parameter passed to [ggplot2::geom_line]
#' (has no effect if `show_all_curves = FALSE`)
#' @param show_quantiles whether to show point-wise (over time) quantiles
#' @param log_x whether to log-transform the x-axis
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
#' plot2 <- graph.curve.params(curve, show_all_curves = TRUE)
#' show(plot2)
#'
graph.curve.params <- function( # nolint: object_name_linter
  curve_params,
  antigen_isos = unique(curve_params$antigen_iso),
  verbose = FALSE,
  show_quantiles = TRUE,
  show_all_curves = FALSE,
  alpha_samples = 0.3,
  log_x = FALSE
) {
  if (verbose) {
    message(
      "Graphing curves for antigen isotypes: ",
      paste(antigen_isos, collapse = ", ")
    )
  }

  curve_params <- curve_params |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  tx2 <- 10^seq(-1, 3.1, 0.025)

  if (inherits(curve_params$alpha, "units")) {
    tx2 <- tx2 |> units::as_units("days")
  }

  d <- curve_params

  dT <- # nolint: object_linter
    data.frame(t = tx2) |>
    mutate(ID = dplyr::row_number()) |>
    pivot_wider(
      names_from = "ID",
      values_from = "t",
      names_prefix = "time"
    ) |>
    dplyr::slice(
      rep(
        seq_len(dplyr::n()),
        each = nrow(d)
      )
    )


  serocourse_all <-
    cbind(d, dT) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("time"),
      values_to = "t"
    ) |>
    select(-"name") |>
    rowwise() |>
    mutate(
      res = ab_5p(
        .data$t,
        .data$y0,
        .data$y1,
        .data$t1,
        .data$alpha,
        .data$r
      )
    ) |>
    ungroup()

  if (verbose) message("starting to compute quantiles")
  serocourse_sum <- serocourse_all |>
    summarise(
      .by = c("antigen_iso", "t"),
      res.med = quantile(.data$res, 0.5),
      res.low = quantile(.data$res, 0.025),
      res.high = quantile(.data$res, 0.975),
      res.p75 = quantile(.data$res, 0.75),
      res.p25 = quantile(.data$res, 0.25),
      res.p10 = quantile(.data$res, 0.10),
      res.p90 = quantile(.data$res, 0.90)
    ) |>
    pivot_longer(
      names_to = "quantile",
      cols = c(
        "res.med",
        "res.low",
        "res.high",
        "res.p25",
        "res.p75",
        "res.p10",
        "res.p90"
      ),
      names_prefix = "res.",
      values_to = "res"
    )


  range <-
    serocourse_sum |>
    dplyr::filter(.data$quantile %in% c("med", "p10", "p90")) |>
    dplyr::summarize(
      min = min(.data$res, 0.9),
      max = max(.data$res, 2000)
    )


  plot1 <-
    serocourse_sum |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$t,
                 y = .data$res) +
    ggplot2::facet_wrap(
      ~ .data$antigen_iso,
      ncol = 2
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line = ggplot2::element_line()) +
    ggplot2::labs(x = "Days since fever onset",
                  y = "ELISA units",
                  col = if_else(show_all_curves, "MCMC chain", "")) +
    ggplot2::theme(legend.position = "bottom")

  if (show_all_curves) {

    range <-
      serocourse_all |>
      dplyr::summarize(
        min = min(.data$res, 0.9),
        max = max(.data$res, 2000)
      )

    group_vars <-
      c("iter", "chain") |>
      intersect(names(serocourse_all))

    if (length(group_vars) > 1) {
      serocourse_all <-
        serocourse_all |>
        mutate(
          iter = interaction(across(all_of(group_vars)))
        )
      plot1 <-
        plot1 +
        geom_line(
          data = serocourse_all,
          alpha = alpha_samples,
          aes(
            color = .data$chain |> factor(),
            group = .data$iter
          )
        ) +
        ggplot2::expand_limits(y = range)
    } else {

      plot1 <-
        plot1 +
        geom_line(data = serocourse_all,
                  alpha = alpha_samples,
                  aes(group = .data$iter)) +
        ggplot2::expand_limits(y = range)

    }

  }

  plot1 <-
    plot1 +
    ggplot2::scale_y_log10(
      limits = unlist(range),
      labels = scales::label_comma(),
      minor_breaks = NULL
    )

  if (log_x) {
    plot1 <-
      plot1 +
      ggplot2::scale_x_log10(
        labels = scales::label_comma(),
        minor_breaks = NULL
      )
  }

  if (show_quantiles) {
    plot1 <-
      plot1 +
      ggplot2::geom_line(
        ggplot2::aes(col = "median"),
        data = serocourse_sum |> filter(.data$quantile == "med"),
        linewidth = 1
      ) +
      ggplot2::geom_line(
        ggplot2::aes(col = "10% quantile"),
        data = serocourse_sum |> filter(quantile == "p10"),
        linewidth = .5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(col = "90% quantile"),
        data = serocourse_sum |> filter(quantile == "p90"),
        linewidth = .5
      )
  }

  return(plot1)

}
