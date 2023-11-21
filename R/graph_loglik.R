graph_loglik = function(
  lambda.start = .1,
  ...,
    x =
      c(
        lambda.start,
        .00001,
        .0001,
        seq(.001, .01, by = .001),
        c(.01, .015, .02, .025, .03),
        seq(.04, .5, by = .01)) |>
      unique() |>
      sort())
{

  # ll = function(x)
  # {
  #
  #
  #   nll_vec(
  #     log.lambda = x,
  #     data = csdataL |> rename(y = value, a = age) |> split(~antigen_iso),
  #     antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  #     curve_params = dmcmc |>
  #       mutate(
  #         alpha = .data$alpha * 365.25,
  #         d = .data$r - 1) |> split(~antigen_iso) ,
  #     noise_params = cond |> split(~antigen_iso),
  #     verbose = TRUE
  #   )
  # }

  tibble(
    x = x,
    y = -.nll_vec(x |> log(), ...)
  ) |>
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("lambda (events/person-year)") +
    ggplot2::ylab("log(likelihood)") +
    ggplot2::theme_bw() +
    ggplot2::geom_point(
      data = tibble(
        x = lambda.start,
        y = -.nll_vec(lambda.start |> log(), ...)),
      aes(x = x, y = y, col = "lambda.start")
    )

}
