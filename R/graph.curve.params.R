#' Graph estimated antibody decay curve
#'
#' @param curve_params a [data.frame()] containing MCMC samples of antibody decay curve parameters
#' @param verbose verbose output
#' @param antigen_isos antigen isotypes
#' @returns a [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' \donttest{
#' curve_params <- readRDS(url("https://osf.io/download/rtw5k/"))
#'
#' plot1 <- graph.curve.params(curve_params)
#'
#' print(plot1)
#' }
graph.curve.params <- function(
    curve_params,
    antigen_isos = unique(curve_params$antigen_iso),
    verbose = FALSE) {
  if (verbose) {
    message(
      "Graphing curves for antigen isotypes: ",
      paste(antigen_isos, collapse = ", ")
    )
  }

  curve_params <- curve_params %>%
    dplyr::filter(.data$antigen_iso %in% antigen_isos)

  day2yr <- 365.25

  tx2 <- 10^seq(-1, 3.1, 0.025)

  bt <- function(y0, y1, t1) {
    log(y1 / y0) / t1
  }

  # uses r > 1 scale for shape
  ab <- function(t, y0, y1, t1, alpha, shape) {
    beta <- bt(y0, y1, t1)

    yt <- 0

    if (t <= t1) {
      yt <- y0 * exp(beta * t)
    }

    if (t > t1) {
      yt <- (y1^(1 - shape) - (1 - shape) * alpha * (t - t1))^(1 / (1 - shape))
    }

    return(yt)
  }


  d <- curve_params
  # %>%
  #   mutate(alpha = .data$alpha / day2yr)



  dT <-
    data.frame(t = tx2) %>%
    mutate(ID = 1:n()) %>%
    pivot_wider(
      names_from = .data$ID,
      values_from = .data$t,
      names_prefix = "time"
    ) %>%
    dplyr::slice(
      rep(1:dplyr::n(),
        each = nrow(d)
      )
    )


  serocourse.all <-
    cbind(d, dT) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("time"),
      values_to = "t"
    ) %>%
    select(-"name") %>%
    rowwise() %>%
    mutate(res = ab(
      .data$t,
      .data$y0,
      .data$y1,
      .data$t1,
      .data$alpha,
      .data$r
    )) %>%
    ungroup()

  if (verbose) message("starting to compute quantiles")
  serocourse.sum <- serocourse.all %>%
    summarise(
      .by = c("antigen_iso", "t"),
      res.med = quantile(.data$res, 0.5),
      res.low = quantile(.data$res, 0.025),
      res.high = quantile(.data$res, 0.975),
      res.p75 = quantile(.data$res, 0.75),
      res.p25 = quantile(.data$res, 0.25),
      res.p10 = quantile(.data$res, 0.10),
      res.p90 = quantile(.data$res, 0.90)
    ) %>%
    pivot_longer(
      names_to = "quantile",
      cols = c(
        "res.med",
        "res.low",
        "res.high",
        "res.p25",
        "res.p75",
        "res.p10",
        "res.p90"
      ),
      names_prefix = "res.",
      values_to = "res"
    )



  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = serocourse.sum %>%
        filter(.data$quantile == "med"),
      ggplot2::aes(
        x = .data$t,
        y = .data$res
      ),
      linewidth = 1
    ) +
    ggplot2::geom_line(
      data = serocourse.sum %>% filter(quantile == "p10"),
      ggplot2::aes(
        x = .data$t,
        y = .data$res
      ),
      linewidth = .5
    ) +
    ggplot2::geom_line(
      data = serocourse.sum %>%
        filter(quantile == "p90"),
      ggplot2::aes(
        x = .data$t,
        y = .data$res
      ),
      linewidth = .5
    ) +
    ggplot2::facet_wrap(
      ~ .data$antigen_iso,
      ncol = 2
    ) +
    ggplot2::scale_y_log10(
      limits = c(0.9, 2000),
      breaks = c(1, 10, 100, 1000),
      minor_breaks = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line = ggplot2::element_line()) +
    ggplot2::labs(x = "Days since fever onset", y = "ELISA units")
}

# ggplot() +
#   geom_line(data = serocourse.all, aes(x= t, y = res, group = iter)) +
#   facet_wrap(~antigen_iso, ncol=2) +
#   scale_y_log10(limits = c(0.9, 2000), breaks = c(1, 10, 100, 1000), minor_breaks = NULL) +
#   theme_minimal()  +
#   theme(axis.line=element_line()) +
#   labs(x="Days since fever onset", y="ELISA units")
