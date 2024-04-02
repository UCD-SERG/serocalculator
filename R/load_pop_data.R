#' Load a cross-sectional antibody survey data set
#'
#' @param file_path path to an RDS file containing a cross-sectional antibody survey data set, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#'
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' xs_data = load_pop_data(
#' )
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

  # create pop_data class
  class(pop_data) =
    c("pop_data", class(pop_data))

  # define pop_data get age function
  get_age <- function(x)
  {
      UseMethod("get_age",x)
  }

  # implementation of get_age function
  get_age.pop_data <- function(){

    # get column(s) matching 'age'
    age_var <- pop_data %>%
      select(contains("age")) %>%
      names()

    # set age attribute on pop_data
    attributes(pop_data)$age_var <- age_var

    # return age
    return(age_var)
  }

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
