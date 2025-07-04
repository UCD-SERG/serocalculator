#' Barplot method for `summary.seroincidence.by` objects
#'
#' @param object a `summary.seroincidence.by` object (generated by applying the
#' `summary()` method to the output of [est_seroincidence_by()]).
#' @param yvar the name of a stratifying variable in `object`.
#' @param color_var
#' [character] the name of the fill color variable (e.g., "Country").
#' @param alpha
#' transparency for the bars (1 = no transparency, 0 = fully transparent).
#' @param CIs [logical], if `TRUE`, add CI error bars.
#' @param title a title for the final plot.
#' @param xlab a label for the x-axis of the final plot.
#' @param ylab a label for the y-axis of the final plot.
#' @param fill_lab fill label.
#' @param color_palette optional color palette for bar color.
#' @param ... unused.
#'
#' @return a [ggplot2::ggplot()] object.
#' @export
#' @keywords internal
#'
strat_ests_barplot <- function(
    object,
    yvar,
    color_var = NULL,
    alpha = 0.7,
    CIs = FALSE, # nolint: object_name_linter
    title = NULL,
    xlab = "Seroconversion rate per 1000 person-years",
    ylab = yvar,
    fill_lab = NULL,
    color_palette = NULL,
    ...) {

  # Check if yvar exists in the dataset
  if (!is.element(yvar, names(object))) {
    cli::cli_abort(
      class = "unavailable_yvar",
      message = c(
        "The variable `{yvar}` specified by argument `yvar`
        does not exist in `object`.",
        "Please choose a column that exists in `object`."
      )
    )
  }

  plot1 <- ggplot2::ggplot(object) +
    ggplot2::aes(
      y = forcats::fct_rev(.data[[yvar]]),
      x = .data$incidence.rate * 1000,
      fill = if (!is.null(color_var)) .data[[color_var]] else NULL
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge(),
      show.legend = !is.null(color_var),
      alpha = alpha
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      fill = fill_lab
    ) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 11),
      axis.text.x = ggplot2::element_text(size = 11)
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 10))

  # Add error bars if CIs are requested
  if (CIs && ("CI.lwr" %in% names(object)) && ("CI.upr" %in% names(object))) {
    plot1 <- plot1 +
      ggplot2::geom_errorbar(ggplot2::aes(
        xmin = .data$CI.lwr * 1000,
        xmax = .data$CI.upr * 1000
      ), width = 0.2, position = ggplot2::position_dodge(width = 0.9))
  }

  # Apply custom color palette if provided
  if (!is.null(color_palette) && !is.null(color_var)) {
    plot1 <- plot1 + ggplot2::scale_fill_manual(values = color_palette)
  }

  return(plot1)
}
