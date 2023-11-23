graph_loglik = function(
    lambda.start = .1,
    ...,
    x.max = .6,
    x =
      c(
        lambda.start,
        .00001,
        .0001,
        seq(.001, .01, by = .001),
        c(.01, .015, .02, .025, .03),
        seq(.04, .1, by = .01),
        seq(.2, x.max, by = .1)) |>
      unique() |>
      sort())
{

  plot1 = tibble(
    x = x,
    y = llik(lambda = x, ...)
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("incidence rate (events per person:year)") +
    ggplot2::ylab("log(likelihood)") +
    ggplot2::theme_bw() +
    ggplot2::geom_point(
      data = tibble(
        x = lambda.start,
        y = llik(lambda = lambda.start, ...)),
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        col = "lambda.start" |> factor(levels = c("lambda.start", "est.incidence")))
    ) +
    ggplot2::labs(col = "") +
    ggplot2::theme(legend.position="bottom")

  return(plot1)
}
