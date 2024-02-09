#' Plot the log-likelihood curve for the incidence rate estimate
#'
#' @param object a `seroincidence` object (from [est.incidence()])
#' @param log_x should the x-axis be on a logarithmic scale (`TRUE`) or linear scale (`FALSE`, default)?
#' @param ... unused
#'
#' @return a [ggplot2::ggplot()]
#' @export
#' @examples
#'  #generate cross-sectional data
#' csdata <- sim.cs(
#'   curve_params = dmcmc,
#'   lambda = lambda,
#'   n.smpl = nrep,
#'   age.rng = lifespan,
#'   antigen_isos = antibodies,
#'  n.mc = 0,
#'  renew.params = TRUE,
#'  add.noise = TRUE,
#'  noise_limits = dlims,
#'  format = "long"
#' )
#'
#' #load in longitudinal parameters
#' dmcmc =
#'   "https://osf.io/download/rtw5k" |>
#'   load_curve_params()
#'
#' #Load noise params
#'   cond <- tibble(
#'   antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
#'   nu = c(0.5, 0.5),                          # Biologic noise (nu)
#'   eps = c(0, 0),                             # M noise (eps)
#'   y.low = c(1, 1),                           # low cutoff (llod)
#'   y.high = c(5e6, 5e6))                      # high cutoff (y.high)
#'
#' #Calculate seroincidence
#' est1 = est.incidence(
#'   pop_data = csdata,
#'   curve_params = dmcmc,
#'   noise_params = cond,
#'   lambda_start = .1,
#'   build_graph = T,
#'   verbose = T, # print updates as the function runs
#'   print_graph = F, # display the log-likelihood curve while `est.incidence()` is running
#'   antigen_isos = antibodies)
#'
#' #plot the log-likelihood curve
#'   autoplot(est1)
#'
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
        labels = scales::label_comma())
  }

  return(to_return)
}
