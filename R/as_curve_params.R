
as_curve_params <- function(object, antigen_isos = NULL, ...) {

  curve_params =
    object %>%
    tibble::as_tibble()

  class(curve_params) =
    c("curve_params", class(curve_params))

  if(is.null(antigen_isos))
  {
    antigen_isos = unique(curve_params$antigen_iso)
  } else
  {
    stopifnot(all(is.element(antigen_isos, curve_params$antigen_iso)))

  }

  attr(curve_params, "antigen_isos") = antigen_isos

  return(curve_params)
}
