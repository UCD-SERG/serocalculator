#' Plot distribution of antibodies
#' @description
#' `autoplot()` method for `pop_data` objects
#'
#' @param object A `pop_data` object (from [load_pop_data()])
#' @param log whether to show antibody responses on logarithmic scale
#' @param strata the name of a variable in `pop_data` to stratify by (or `NULL` for no stratification)
#' @param ... unused
#'
#' @return a [ggplot2::ggplot] object
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' xs_data <- "https://osf.io/download//n6cp3/" %>%
#' load_pop_data() %>%
#' clean_pop_data()
#'
#' xs_data %>% autoplot(strata = "Country")
#'
autoplot.pop_data = function(
    object,
    log =  FALSE,
    strata = NULL,
    ...)
{

  plot1 =
    object %>%
    ggplot2::ggplot(aes(x = .data$value)) +
    ggplot2::theme_linedraw() +
    ggplot2::facet_wrap(~antigen_iso, nrow = 3)

  if(!is.null(strata))
  {
    plot1 = plot1 +
      ggplot2::geom_density(
        ggplot2::aes(fill = get(strata)),
        alpha = .6,
        color = "black") +
      ggplot2::theme_linedraw()
  } else
  {
    plot1 = plot1 +
      ggplot2::geom_density(
        # aes(fill = get(strata)),
        alpha = .6,
        color = "black") +
      ggplot2::theme_linedraw()
}


  if(log)
  {
    plot1 = plot1 +
      ggplot2::scale_x_log10() +
      ggplot2::theme_linedraw() +
      ggplot2::labs(
        title = "Distribution of Cross-sectional Antibody Responses (Log transformed)",
        x = "Log10(Antibody Response Value)",
        y = "Frequency"
      )
  } else
  {
    plot = plot1 +
      ggplot2::labs(
        title = "Distribution of Cross-sectional Antibody Responses",
        x = "Antibody Response Value",
        y = "Frequency"
      ) + ggplot2::theme_linedraw()

  }

  return(plot1)
  }
