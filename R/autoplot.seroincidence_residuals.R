#' Plot Residual Diagnostics for Seroincidence Model
#'
#' @description
#' Creates a panel of diagnostic plots for model residuals, including
#' residuals vs fitted, Q-Q plot, residuals by antigen, and histogram.
#'
#' @param object a `seroincidence_residuals` object from [calculate_residuals()]
#' @param ... unused
#'
#' @details
#' Produces a 2x2 panel of plots:
#' 1. Residuals vs Fitted: Check for heteroscedasticity
#' 2. Q-Q Plot: Check for normality of standardized residuals
#' 3. Residuals by Antigen: Box plots of residuals by antigen
#' 4. Histogram: Distribution of residuals
#'
#' @return
#' A [patchwork::patchwork] plot object with four panels
#'
#' @export
autoplot.seroincidence_residuals <- function(object, ...) {

  if (!inherits(object, "seroincidence_residuals")) {
    stop("`object` must be a seroincidence_residuals object")
  }

  # Plot 1: Residuals vs Fitted
  p1 <- ggplot2::ggplot(
    object,
    ggplot2::aes(x = .data$predicted, y = .data$residual)
  ) +
    ggplot2::geom_point(alpha = 0.5, size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::facet_wrap(~.data$antigen_iso) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Predicted Antibody Level",
      y = "Residual"
    ) +
    ggplot2::theme_minimal()

  # Plot 2: Q-Q Plot for standardized residuals
  if ("standardized_residual" %in% names(object)) {
    std_resid <- object$standardized_residual
    qq_data <- tibble::tibble(
      sample = stats::qqnorm(std_resid, plot.it = FALSE)$x,
      theoretical = stats::qqnorm(std_resid, plot.it = FALSE)$y
    )

    p2 <- ggplot2::ggplot(
      qq_data,
      ggplot2::aes(x = .data$theoretical, y = .data$sample)
    ) +
      ggplot2::geom_point(alpha = 0.5, size = 2) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = "Normal Q-Q Plot",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      ggplot2::theme_minimal()
  } else {
    # Fallback if standardized residuals not available
    p2 <- ggplot2::ggplot() +
      ggplot2::geom_text(
        ggplot2::aes(x = 0.5, y = 0.5, label = "Standardized residuals not available"),
        size = 5
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Normal Q-Q Plot")
  }

  # Plot 3: Residuals by Antigen
  p3 <- ggplot2::ggplot(
    object,
    ggplot2::aes(x = .data$antigen_iso, y = .data$residual, fill = .data$antigen_iso)
  ) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "Residuals by Antigen",
      x = "Antigen-Isotype",
      y = "Residual"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # Plot 4: Histogram of Residuals
  p4 <- ggplot2::ggplot(
    object,
    ggplot2::aes(x = .data$residual)
  ) +
    ggplot2::geom_histogram(
      bins = 30,
      alpha = 0.7,
      fill = "#1b9e77",
      color = "black"
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "Histogram of Residuals",
      x = "Residual",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal()

  # Combine plots
  combined_plot <- (p1 / p3) | (p2 / p4)

  return(combined_plot)
}
