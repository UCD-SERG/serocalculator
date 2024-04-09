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
load_pop_data = function(file_path, antigen_isos = NULL, age, id, value)
{
  if(file_path %>% substr(1,4) == "http")
  {
    file_path = url(file_path)
  }

  pop_data =
    file_path %>% readRDS() %>%
    tibble::as_tibble()

  # set pop_data class
  attr(pop_data, "class") = c('pop_data',class(pop_data))

  if(is.null(antigen_isos))
  {
    antigen_isos = unique(pop_data$antigen_iso)
  } else
  {
    stopifnot(all(is.element(antigen_isos, pop_data$antigen_iso)))
  }

  attr(pop_data, "antigen_isos") = antigen_isos

  ##### AGE
  if(age %in% colnames(pop_data))
  {
    attr(pop_data, "age_var") = age
  } else
  {
    # search age variable from pop_data
    age_var <- pop_data %>%
      select(contains("age",ignore.case = TRUE) & ends_with("e", ignore.case = TRUE)) %>%
      names()

    if(length(age_var) > 0)
    {
      attr(pop_data, "age_var") <- age_var
    } else
    {
      cli::cli_abort(
        '`age = "{age}"` is not a valid input;
      the age column in not available in the data set')
    }

  }

  ##### INDEX
  if(id %in% colnames(pop_data))
  {
    attr(pop_data, "age_var") <- id
  } else
  {
    # search index variable from pop_data
    id_var <- pop_data %>%
      select(contains("id",ignore.case = TRUE)) %>%
      names()

    if(length(id_var) > 0)
    {
      attr(pop_data, "id_var") <- id_var
    } else
    {
      cli::cli_abort(
        '`id = "{id}"` is not a valid input;
      the id column in not available in the data set')
    }

  }

  ##### VALUE
  if(value %in% colnames(pop_data))
  {
    attr(pop_data, "value_var") <- value
  } else
  {
    # search value variable from pop_data
    value_var <- pop_data %>%
      select(contains("result",ignore.case = TRUE)) %>%
      names()

    if(length(value_var) > 0)
    {
      attr(pop_data, "value_var") <- value_var
    } else
    {
      cli::cli_abort(
        '`value = "{value}"` is not a valid input;
      the value column in not available in the data set')
    }

  }

  return(pop_data)

}

#' @export
get_age <- function(x)
{
  UseMethod("get_age",x)
}

#' @export
get_age.pop_data <- function(obj){

  # get age data
  age_data <- obj %>% select(attributes(obj)$age_var)

  return(age_data)
}

#' @export
get_value.pop_data <- function(obj){

  # get age data
  value_data <- obj %>% select(attributes(obj)$value_var)

  return(value_data)
}

#' @export
get_id.pop_data <- function(obj){

  # get age data
  id_data <- obj %>% select(attributes(obj)$id_var)

  return(id_data)
}
