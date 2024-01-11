#' Plot distribution of antibodies
#'
#' @param pop_data A `pop_data` object (from [load_pop_data()])
#' @param log whether to show antibody responses on logarithmic scale
#' @param strata the name of a variable in `pop_data` to stratify by (or `NULL` for no stratification)
#'
#' @return a [ggplot2::ggplot] object
#' @export
autoplot.pop_data = function(
    pop_data,
    log =  FALSE,
    strata = NULL)
{

  plot1 =
    pop_data |>
    ggplot(aes(x = value)) +
    theme_minimal() +
    facet_wrap(~antigen_iso, nrow = 3)

  if(!is.null(strata))
  {
    plot1 = plot1 +
      geom_density(
        aes(fill = get(strata)),
        alpha = .6,
        color = "black")
  } else
  {
    plot1 = plot1 +
      geom_density(
        # aes(fill = get(strata)),
        alpha = .6,
        color = "black")
}


  if(log)
  {
    plot1 = plot1 +
      scale_x_log10() +
      labs(
        title = "Distribution of Cross-sectional Antibody Responses (Log transformed)",
        x = "Log10(Antibody Response Value)",
        y = "Frequency"
      )
  } else
  {
    plot = plot1 +
      labs(
        title = "Distribution of Cross-sectional Antibody Responses",
        x = "Antibody Response Value",
        y = "Frequency"
      )

  }

  return(plot1)
  }