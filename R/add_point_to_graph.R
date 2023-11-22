add_point_to_graph = function(
 graph,
 fit,
 name = "est.incidence",
 point_data = tibble(
   x = exp(fit$estimate),
   y = -.nll_vec(fit$estimate, ...),
   label = "est.incidence"),
 ...
)
{

  graph =
    graph +
    ggplot2::geom_point(
      data = point_data,
      ggplot2::aes(x = x, y = y, col = label)
    )

}
