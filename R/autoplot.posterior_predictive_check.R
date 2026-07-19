#' Plot Posterior Predictive Check Results
#'
#' @description
#' Creates visualizations for posterior predictive check results,
#' comparing observed data to simulated data from the fitted model.
#'
#' @param object a `posterior_predictive_check` object
#' @param ... unused
#'
#' @details
#' Produces multiple plots in a grid:
#' * Distribution of observed vs simulated antibody levels by antigen
#' * Boxplots by age groups
#' * Q-Q plots comparing quantiles
#'
#' @return
#' A [patchwork::patchwork] plot object
#'
#' @export
autoplot.posterior_predictive_check <- function(object, ...) {

  if (!inherits(object, "posterior_predictive_check")) {
    stop("`object` must be a posterior_predictive_check object")
  }

  # Get data
  observed <- object$observed
  simulated <- object$simulated
  antigen_isos <- object$antigen_isos

  age_var <- get_age_var(observed)
  values_var <- get_values_var(observed)

  # Plot 1: Density plots comparing observed vs simulated distributions
  density_plots <- lapply(antigen_isos, function(ag) {
    obs_data <- observed |>
      dplyr::filter(.data$antigen_iso == ag) |>
      dplyr::rename(value = dplyr::all_of(values_var)) |>
      dplyr::mutate(source = "Observed")

    sim_data <- do.call(dplyr::bind_rows, simulated) |>
      dplyr::filter(.data$antigen_iso == ag) |>
      dplyr::rename(value = dplyr::all_of(values_var)) |>
      dplyr::mutate(source = "Simulated")

    combined <- dplyr::bind_rows(obs_data, sim_data)

    ggplot2::ggplot(combined, ggplot2::aes(x = .data$value, fill = .data$source)) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::scale_fill_manual(
        values = c("Observed" = "#1b9e77", "Simulated" = "#d95f02")
      ) +
      ggplot2::labs(
        title = ag,
        x = "Antibody Level",
        y = "Density",
        fill = "Source"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5, size = 10)
      )
  })

  # Plot 2: Boxplots by age groups
  obs_with_age <- observed |>
    dplyr::mutate(
      age_group = cut(.data[[age_var]],
        breaks = seq(
          floor(min(.data[[age_var]], na.rm = TRUE)),
          ceiling(max(.data[[age_var]], na.rm = TRUE)),
          by = 2
        ),
        include.lowest = TRUE
      ),
      source = "Observed"
    ) |>
    dplyr::rename(value = dplyr::all_of(values_var))

  sim_combined <- do.call(dplyr::bind_rows, simulated)
  sim_with_age <- sim_combined |>
    dplyr::mutate(
      age_group = cut(.data[[age_var]],
        breaks = seq(
          floor(min(.data[[age_var]], na.rm = TRUE)),
          ceiling(max(.data[[age_var]], na.rm = TRUE)),
          by = 2
        ),
        include.lowest = TRUE
      ),
      source = "Simulated"
    ) |>
    dplyr::rename(value = dplyr::all_of(values_var))

  combined_age <- dplyr::bind_rows(obs_with_age, sim_with_age) |>
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  boxplot_by_age <- ggplot2::ggplot(
    combined_age,
    ggplot2::aes(x = .data$age_group, y = .data$value, fill = .data$source)
  ) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::facet_wrap(~.data$antigen_iso) +
    ggplot2::scale_fill_manual(
      values = c("Observed" = "#1b9e77", "Simulated" = "#d95f02")
    ) +
    ggplot2::labs(
      title = "Antibody Levels by Age Group",
      x = "Age Group (years)",
      y = "Antibody Level",
      fill = "Source"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Combine plots
  combined_plot <- patchwork::wrap_plots(density_plots, ncol = 2) /
    boxplot_by_age +
    patchwork::plot_layout(heights = c(1, 1), guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  return(combined_plot)
}
