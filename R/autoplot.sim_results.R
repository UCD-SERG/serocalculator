autoplot.sim_results <- function(
    object,
    statistic = "Standard_Error")
{
  object |>
    ggplot2::ggplot() +
    aes(x = .data$Sample_Size,
        group = .data$true_lambda,
        y = .data[[statistic]]) +
    geom_point() +
    geom_line()
}
