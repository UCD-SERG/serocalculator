#' Graph estimated antibody decay curves
#'
#' @param object
#' a [data.frame()] containing MCMC samples of antibody decay curve parameters
#' @param verbose verbose output
#' @param antigen_isos antigen isotypes to analyze
#' (can subset `object`)
#' @param alpha_samples `alpha` parameter passed to [ggplot2::geom_line]
#' (has no effect if `iters_to_graph` is empty)
#' @param quantiles Optional [numeric] [vector] of point-wise (over time)
#' quantiles to plot (e.g., 10%, 50%, and 90% = `c(0.1, 0.5, 0.9)`).
#' If `NULL`, no quantile lines are shown.
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`)
#' or linear scale (`FALSE`, default)?
#' @param log_y should the Y-axis be on a logarithmic scale
#' (default, `TRUE`) or linear scale (`FALSE`)?
#' @param chain_color logical, if `TRUE` (default), MCMC chain lines
#' are colored by chain.
#' If `FALSE`, all MCMC chain lines are black.
#' @inheritParams plot_curve_params_one_ab
#' @param ... not currently used
#' @returns a [ggplot2::ggplot()] object showing the antibody dynamic
#' kinetics of selected antigen/isotype combinations, with optional posterior
#' distribution quantile curves.
#' @export
#' @inherit plot_curve_params_one_ab details
#'
#' @example inst/examples/exm-graph.curve.params.R
#'

graph.curve.params <- function( # nolint: object_name_linter
  object,
  antigen_isos = unique(object$antigen_iso),
  verbose = FALSE,
  quantiles = c(0.1, 0.5, 0.9),
  alpha_samples = 0.3,
  chain_color = TRUE,
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
    dplyr::select(-"name") |>
    dplyr::rowwise() |>
    dplyr::mutate(res = ab1(
      .data$t,
      .data$y0,
      .data$y1,
      .data$t1,
      .data$alpha,
      .data$r
    )) |>
    ungroup()

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
      tidyr::unnest(c("quantiles_df"))
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
      x = "Days since fever onset",
      y = "ELISA units",
      col = if_else(
        length(iters_to_graph) > 0,
        "MCMC chain",
        ""
      )
    ) +
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

      if (chain_color) {
        plot1 <-
          plot1 +
          geom_line(
            data = sc_to_graph,
            alpha = alpha_samples,
            aes(
              color = .data$chain |> factor(),
              group = .data$group
            )
          )
      } else {
        plot1 <-
          plot1 +
          geom_line(
            data = sc_to_graph,
            alpha = alpha_samples,
            color = "black",
            aes(
              group = .data$group
            )
          )
      }

    } else {
      plot1 <-
        plot1 +
        geom_line(data = sc_to_graph,
                  alpha = alpha_samples,
                  aes(group = .data$iter)) +
        ggplot2::expand_limits(y = range)

    }
    plot1 <-
      plot1 + ggplot2::expand_limits(y = unlist(range))
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

  if (!is.null(quantiles)) {
    plot1 <- plot1 +
      ggplot2::geom_line(
        data = serocourse_sum,
        aes(
          color = paste0(.data$quantile * 100, "% quantile"),
          group = .data$quantile
        ),
        linewidth = 0.75
      )

    if (length(iters_to_graph) > 0) {
      if (chain_color) {
        plot1 <- plot1 +
          ggplot2::labs(col = "MCMC chain")
      } else {
        plot1 <- plot1 +
          ggplot2::labs(col = "")
      }
    } else {
      plot1 <- plot1 + ggplot2::labs(col = "Quantile")
    }
  }


  return(plot1)
}
