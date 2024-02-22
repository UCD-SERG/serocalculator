#' Load a cross-sectional antibody survey data set
#'
#' @param file_path path to an RDS file containing a cross-sectional antibody survey data set, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#'
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' xs_data = load_pop_data("https://osf.io/download//n6cp3/")
#' print(xs_data)
#'
#'
load_pop_data = function(file_path, antigen_isos = NULL)
{
  if(file_path %>% substr(1,4) == "http")
  {
    file_path = url(file_path)

  }

  pop_data =
    file_path %>% readRDS() %>%
    tibble::as_tibble()

  class(pop_data) =
    c("pop_data", class(pop_data))

  if(is.null(antigen_isos))
  {
    antigen_isos = unique(pop_data$antigen_iso)
  } else
  {
    stopifnot(all(is.element(antigen_isos, pop_data$antigen_iso)))

  }

  attr(pop_data, "antigen_isos") = antigen_isos

  return(pop_data)

}
