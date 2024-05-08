#' Plot distribution of antibodies
#' @description
#' `autoplot()` method for `pop_data` objects
#'
#' @param object A `pop_data` object (from [load_pop_data()])
#' @param log whether to show antibody responses on logarithmic scale
#' @param strata the name of a variable in `pop_data` to stratify by (or `NULL` for no stratification)
#' @param ... unused
#' @param type an option to choose type of chart: the current options are `"density"` or `"age-scatter"`
#'
#' @return a [ggplot2::ggplot] object
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' xs_data <- "https://osf.io/download//n6cp3/" %>%
#'   load_pop_data() %>%
#'   clean_pop_data()
#'
#' xs_data %>% autoplot(strata <- "Country", type = "density")
#' xs_data %>% autoplot(strata <- "Country", type = "age-scatter")
#' @export
autoplot.pop_data <- function(
    object,
    log = FALSE,
    type = "density",
    strata = NULL,
    ...) {

  if (type == "age-scatter") {
    age_scatter(object, strata)
  } else if (type == "density") {
    density_plot(object, strata, log)
  } else {
    cli::cli_abort(
      '`type = "{type}"` is not a valid input;
      the currently available options for `type` are "density" or "age-scatter"')
  }
}

age_scatter <- function(
    object,
    strata = NULL) {
  # create default plotting

  if(is.null(strata))
  {
    plot1 <-
      object %>%
      ggplot2::ggplot(aes(x = .data$age, y = .data$value))
  } else
  {
    plot1 <-
      object %>%
      ggplot2::ggplot(aes(x = .data$age, y = .data$value, col = get(strata))) +
      ggplot2::labs(colour = strata)
  }

  plot1 <- plot1 +
    ggplot2::theme_linedraw() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_point(size = .6, alpha = .7) +
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      formula = y ~ x,
      na.rm = TRUE) +
    ggplot2::labs(
      title = "Quantitative Antibody Responses by Age",
      x = "Age",
      y = "Antibody Response Value"
    )

  return(plot1)
}

# density plotting function
density_plot <- function(
    object,
    strata = NULL,
    log = FALSE) {
  plot1 <-
    object %>%
    ggplot2::ggplot(aes(x = .data$value)) +
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
    plot1 <- plot1 +
      ggplot2::scale_x_log10() +
      ggplot2::labs(
        title = "Distribution of Cross-sectional Antibody Responses (Log transformed)",
        x = "Log10(Antibody Response Value)",
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
