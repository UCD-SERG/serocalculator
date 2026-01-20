# density plotting function
density_plot <- function(
    object,
    strata = NULL,
    log = FALSE,
    value_var = object |> get_values_var()) {
  plot1 <-
    object |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[value_var]]) +
    ggplot2::theme_linedraw() +
    ggplot2::facet_wrap(~antigen_iso, nrow = 3)

  if (is.null(strata)) {
    plot1 <- plot1 +
      ggplot2::geom_density(
        alpha = .6,
        color = "black"
      )
  } else {
    plot1 <- plot1 +
      ggplot2::geom_density(
        alpha = .6,
        color = "black",
        aes(fill = get(strata))
      ) +
      ggplot2::labs(fill = strata)
  }
  if (log) {
    min_nonzero_val <-
      object |>
      get_values() |>
      purrr::keep(~ . > 0) |>
      min()

    max_val <-
      object |>
      get_values() |>
      max()

    breaks1 <- c(0, 10^seq(
      min_nonzero_val |> log10() |> floor(),
      max_val |> log10() |> ceiling()
    ))

    plot1 <- plot1 +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(),
        transform = scales::pseudo_log_trans(
          sigma = min_nonzero_val / 10,
          base = 10
        ),
        breaks = breaks1
      ) +
      ggplot2::labs(
        title = "Distribution of Cross-sectional Antibody Responses",
        x = "Quantitative antibody response",
        y = "Frequency"
      )
  } else {
    plot1 <- plot1 +
      ggplot2::labs(
        title = "Distribution of Cross-sectional Antibody Responses",
        x = "Antibody Response Value",
        y = "Frequency"
      )
  }
  return(plot1)
}
