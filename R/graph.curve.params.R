#' Graph estimated antibody decay curves
#'
#' @param object
#' a [data.frame()] containing MCMC samples of antibody decay curve parameters
#' @param verbose verbose output
#' @param antigen_isos antigen isotypes to analyze
#' (can subset `object`)
#' @param alpha_samples `alpha` parameter passed to [ggplot2::geom_line]
#' (has no effect if `iters_to_graph` is empty)
#' @param show_quantiles whether to show point-wise (over time) quantiles
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`)
#' or linear scale (`FALSE`, default)?
#' @param log_y should the Y-axis be on a logarithmic scale
#' (default, `TRUE`) or linear scale (`FALSE`)?
#' @inheritParams plot_curve_params_one_ab
#' @param ... not currently used
#' @returns a [ggplot2::ggplot()] object
#' @export
#' @inherit plot_curve_params_one_ab details
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
#' plot2 <- graph.curve.params(curve, n_curves = 100)
#' show(plot2)
#'
graph.curve.params <- function( # nolint: object_name_linter
  object,
  antigen_isos = unique(object$antigen_iso),
  verbose = FALSE,
  show_quantiles = TRUE,
  alpha_samples = 0.3,
  log_x = FALSE,
  log_y = TRUE,
  n_curves = 100,
  iters_to_graph = object$iter |> unique() |> head(n_curves),
  ...
) {
  if (verbose) {
    message(
      "Graphing curves for antigen isotypes: ",
      paste(antigen_isos, collapse = ", ")
    )
  }

  object <- object |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  tx2 <- 10^seq(-1, 3.1, 0.025)

  d <- object

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
    mutate(res = ab1(
      .data$t,
      .data$y0,
      .data$y1,
      .data$t1,
      .data$alpha,
      .data$r
    )) |>
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
      min = min(.data$res),
      max = max(.data$res)
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
                  col = if_else(
                    length(iters_to_graph) > 0,
                    "MCMC chain",
                    "")) +
    ggplot2::theme(legend.position = "bottom")

  if (length(iters_to_graph) > 0) {

    sc_to_graph <-
      serocourse_all |>
      filter(.data$iter %in% iters_to_graph)

    range <-
      sc_to_graph |>
      dplyr::summarize(
        min = min(.data$res),
        max = max(.data$res)
      )

    group_vars <-
      c("iter", "chain") |>
      intersect(names(sc_to_graph))

    if (length(group_vars) > 1) {
      sc_to_graph <-
        sc_to_graph |>
        mutate(
          group = interaction(across(all_of(group_vars)))
        )
      plot1 <-
        plot1 +
        geom_line(
          data = sc_to_graph,
          alpha = alpha_samples,
          aes(
            color = .data$chain |> factor(),
            group = .data$group
          )
        ) +
        ggplot2::expand_limits(y = range)
    } else {

      plot1 <-
        plot1 +
        geom_line(data = sc_to_graph,
                  alpha = alpha_samples,
                  aes(group = .data$iter)) +
        ggplot2::expand_limits(y = range)

    }

  }

  if (log_y) {
    plot1 <-
      plot1 +
      ggplot2::scale_y_log10(
        limits = unlist(range),
        labels = scales::label_comma(),
        minor_breaks = NULL
      )
  }

  if (log_x) {
    plot1 <- plot1 +
      ggplot2::scale_x_log10(labels = scales::label_comma())
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
