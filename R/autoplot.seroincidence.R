#' Plot the log-likelihood curve for the incidence rate estimate
#'
#' @param object a `seroincidence` object (from [estimate_scr()])
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @param ... unused
#'
#' @return a [ggplot2::ggplot()]
#' @export
#' @examples
#'\donttest{
#' library(dplyr)
#' library(ggplot2)
#'
#' xs_data <-
#'   sees_pop_data_pk_100
#'
#' curve <-
#'   typhoid_curves_nostrat_100 %>%
#'   filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))
#'
#' noise <-
#'   example_noise_params_pk
#'
#' est1 <- estimate_scr(
#'   pop_data = xs_data,
#'   sr_param = curve,
#'   noise_param = noise,
#'   antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
#'   build_graph = TRUE
#' )
#'
#' # Plot the log-likelihood curve
#' autoplot(est1)
#'}
autoplot.seroincidence =
  function(object, log_x = FALSE, ...)
{
  to_return = attr(object, "ll_graph")

    if (is.null(to_return)) {
      stop(
        "Graphs cannot be extracted; ",
        "`build_graph` was not `TRUE` in the call to `estimate_scr()`"
      )
      figure <- NULL
    }

    if (log_x) {
      to_return <- to_return +
        ggplot2::scale_x_log10(
          labels = scales::label_comma()
        ) +
        ggplot2::theme_linedraw()
    }

    return(to_return)
  }
