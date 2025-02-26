#' Plot distribution of antibodies
#' @description
#' `autoplot()` method for `pop_data` objects
#'
#' @param object A `pop_data` object (from [load_pop_data()])
#' @param log whether to show antibody responses on logarithmic scale
#' @param strata the name of a variable in `pop_data`
#' to stratify by (or `NULL` for no stratification)
#' @param ... unused
#' @param type an option to choose type of chart:
#' the current options are `"density"` or `"age-scatter"`
#'
#' @return a [ggplot2::ggplot] object
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' library(magrittr)
#'
#' xs_data <-
#'   serocalculator_example("example_pop_data.csv") |>
#'   read.csv() |>
#'   as_pop_data()
#'
#' xs_data |> autoplot(strata = "catchment", type = "density")
#' xs_data |> autoplot(strata = "catchment", type = "age-scatter")
#' }
#' @export
autoplot.pop_data <- function(
    object,
    log = FALSE,
    type = "density",
    strata = NULL,
    ...) {
  if (!is.null(strata) && !is.element(strata, names(object))) {
    cli::cli_abort(
      class = "unavailable_strata",
      message = c(
        x = "The variable {.var {strata}} specified by argument {.arg strata}
        does not exist in {.arg object}.",
        i = "Please choose a column that exists in {.arg object}."
      )
    )
  }

  if (type == "age-scatter") {
    age_scatter(object, strata)
  } else if (type == "density") {
    density_plot(object, strata, log)
  } else {
    cli::cli_abort(
      class = "unavailable_type",
      message = c(
        x = "{.fn autoplot.pop_data} does not currently have an option for
         {.arg type} = {.str {type}}.",
        i = "The {.arg type} argument accepts options
        {.str density} or {.str age-scatter}."
      )
    )
  }
}

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
    )

  return(plot1)
}

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
