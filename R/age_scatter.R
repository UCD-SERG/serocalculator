age_scatter <- function(
    object,
    strata = NULL,
    age_var = object |> get_age_var(),
    value_var = object |> get_values_var()) {
  # create default plotting

  biomarker_var <- object |> get_biomarker_names_var()

  if (is.null(strata)) {
    plot1 <-
      object |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = .data[[age_var]],
        y = .data[[value_var]]
      )
  } else {
    plot1 <-
      object |>
      ggplot2::ggplot() +
      ggplot2::aes(
        col = .data[[strata]],
        x = .data[[age_var]],
        y = .data[[value_var]]
      ) +
      ggplot2::labs(colour = strata)
  }

  plot1 <- plot1 +
    ggplot2::theme_linedraw() +
    # ggplot2::scale_y_log10() +

    # avoid log 0 (https://bit.ly/4eqDkT4)
    ggplot2::scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma = 0.01),
      breaks = c(-1, -0.1, 0, 0.1, 1, 10),
      minor_breaks = NULL
    ) +
    ggplot2::geom_point(size = .6, alpha = .7) +
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      formula = y ~ x,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(biomarker_var) +
    ggplot2::labs(
      title = "Quantitative Antibody Responses by Age",
      x = "Age",
      y = "Antibody Response Value"
    ) +
    ggplot2::theme(legend.position = "bottom")

  return(plot1)
}
