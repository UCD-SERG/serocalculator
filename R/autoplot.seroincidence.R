#' Plot the log-likelihood curve for the incidence rate estimate
#'
#' @param object a `seroincidence` object (from [est.incidence()])
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @param ... unused
#'
#' @return a [ggplot2::ggplot()]
#' @export
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' xs_data = load_pop_data("https://osf.io/download//n6cp3/")
#' xs_data = clean_pop_data(xs_data)
#'
#' curve = load_curve_params("https://osf.io/download/rtw5k/")
#' noise = load_noise_params("https://osf.io/download//hqy4v/")
#'
#' est1 = est.incidence(
#'   pop_data = xs_data %>% filter(Country == "Pakistan"),
#'   curve_param = curve,
#'   noise_param = noise %>% filter(Country == "Pakistan"),
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   build_graph = TRUE
#' )

#'
#' #plot the log-likelihood curve
#'   autoplot(est1)
#'}
autoplot.seroincidence =
  function(object, log_x = FALSE, ...)
{
  to_return = attr(object, "ll_graph")

  if(is.null(to_return))
  {
    stop(
      "Graphs cannot be extracted; ",
      "`build_graph` was not `TRUE` in the call to `est.incidence()`")
    figure = NULL
  }

  if(log_x)
  {
    to_return = to_return +
      ggplot2::scale_x_log10(
        labels = scales::label_comma()) +
      ggplot2::theme_linedraw()
  }

  return(to_return)
}