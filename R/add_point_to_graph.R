add_point_to_graph = function(
    graph,
    fit,
    name = "est.incidence",
    point_data =
      tibble(
        x = fit$estimate |> exp(),
        y = llik(.data$x, ...),
        label = "est.incidence"
      ),
    ...
)
{

  graph =
    graph +
    ggplot2::geom_point(
      data = point_data,
      ggplot2::aes(x = .data$x, y = .data$y, col = .data$label)
    )

}
