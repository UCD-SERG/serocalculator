#' Load antibody decay curve parameter samples
#'
#' @param file_path path to an RDS file containing MCMC samples of antibody decay curve parameters `y0`, `y1`, `t1`, `alpha`, and `r`, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#'
#' @returns a `curve_params` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#'
load_curve_params = function(file_path, antigen_isos = NULL)
{
  if(file_path |> substr(1,4) == "http")
  {
    file_path = url(file_path)

  }

  curve_params =
    file_path %>%
    readRDS() |>
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
