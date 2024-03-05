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
    type = 'density',
    strata = NULL,
    ...)
{
  if(type == 'age-scatter')
    {
    plot1 =
      object %>%
      ggplot2::ggplot(aes(x = age,y = value)) +
      ggplot2::theme_linedraw()

    if(is.null(strata))
    {
      plot1 = plot1 +
          ggplot2::geom_point(size=.6, alpha=.7) +
          ggplot2::geom_smooth(method=lm, se=FALSE) +
          scale_y_log10() +
          theme_linedraw()+
          labs(
              title = "Quantitative Antibody Responses by Age",
            x = "Age",
            y = "Value"
            )
    } else
    {
      plot1 = plot1 +
        ggplot2::geom_point(size=.6,
                            alpha=.7,
                            aes(color = get(strata))) +
        ggplot2::geom_smooth(method=lm,
                             se=FALSE,
                             aes(color = get(strata))
                             ) +
        scale_y_log10() +
        labs(
          title = "Quantitative Antibody Responses by Age",
          x = "Age",
          y = "Value",
          colour = strata
          )
    }

    } else

    {
    plot1 =
        object %>%
        ggplot2::ggplot(aes(x = value)) +
        ggplot2::theme_linedraw() +
        ggplot2::facet_wrap(~antigen_iso, nrow = 3)

    if(is.null(strata))
    {
      plot1 = plot1 +
        ggplot2::geom_density(alpha = .6,
                              color = "black")
    } else
    {
      plot1 = plot1 +
        ggplot2::geom_density(
          aes(fill = get(strata)),
          alpha = .6,
          color = "black") +
        ggplot2::labs(fill = strata)
    }
    if(log)
    {
      plot1 = plot1 +
        ggplot2::scale_x_log10() +
        ggplot2::labs(
          title = "Distribution of Cross-sectional Antibody Responses (Log transformed)",
          x = "Log10(Antibody Response Value)",
          y = "Frequency"
        )
    } else
    {
      plot1 = plot1 +
        ggplot2::labs(
          title = "Distribution of Cross-sectional Antibody Responses",
          x = "Antibody Response Value",
          y = "Frequency"
        )
    }
    }

  return(plot1)
}
