#' Load a cross-sectional antibody survey data set
#'
#' @param file_path path to an RDS file containing a cross-sectional antibody survey data set, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#' @param age a[character()] identifying the age column
#' @param id a[character()] identifying the id column
#' @param value a[character()] identifying the value column
#' @param standardize a [logical()]to determine standardization of columns
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' xs_data = load_pop_data(file_path = "https://osf.io/download//n6cp3/",
#'                        age = "Age",
#'                        id = "index_id",
#'                        value = "result")
#'
#' print(xs_data)
load_pop_data = function(file_path,
                         antigen_isos = NULL,
                         age = "Age",
                         id = "index_id",
                         value = "result",
                         standardize = TRUE)
{
  if(file_path %>% substr(1,4) == "http")
  {
    file_path = url(file_path)
  }

  pop_data =
    file_path %>% readRDS() %>%
    tibble::as_tibble()

  # set pop_data class
  attr(pop_data, "class") = c('pop_data', class(pop_data))

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
    attr(pop_data, "age_var") <- age
  } else
  {
    # search age variable from pop_data
    age_var <- pop_data %>%
      select(tidyselect::matches("age",colnames(pop_data),ignore.case = TRUE) & ends_with("e", ignore.case = TRUE)) %>%
      names()

    if(length(age_var) > 0)
    {
      attr(pop_data, "age_var") <- age_var

      # create warning when using searched age instead of provided age
      cli::cli_alert_warning('The provided age attribute "{age}" does not exist.
                        Proceeding to use "{age_var}"')

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
    attr(pop_data, "id_var") <- id


  } else
  {
    # search index variable from pop_data (no need to find ID)
    id_var <- pop_data %>%
      select(tidyselect::matches("\\w*id\\b")) %>%
      names()


    if(length(id_var) > 0)
    {
      attr(pop_data, "id_var") <- id_var

      # create warning when using searched id  instead of provided id
      cli::cli_alert_warning('The provided id attribute "{id}" does not exist.
                        Proceeding to use "{id_var}"')
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
      select(tidyselect::matches("result", ignore.case = TRUE)) %>%
      names()

    if(length(value_var) > 0)
    {
      attr(pop_data, "value_var") <- value_var

      # create warning when using searched age instead of provided age
      cli::cli_alert_warning('The provided age attribute "{value}" does not exist.
                        Proceeding to use "{value_var}"')
    } else
    {
      cli::cli_abort(
        '`value = "{value}"` is not a valid input;
      the value column in not available in the data set')
    }

  }

  # standardize columns
  if(standardize)
  {
    pop_data <- pop_data %>%
                    rename('age' = attributes(pop_data)$age_var,
                           'value' = attributes(pop_data)$value_var,
                           'id' = attributes(pop_data)$id_var)
  }

  return(pop_data)

}

get_age <- function(object, ...)
{
  UseMethod("get_age", object)
}

#' @export
get_age.pop_data <- function(object, ...){

  # get age data
  age_data <- object %>% pull('age')

  return(age_data)
}

get_value <- function(object, ...)
{
  UseMethod("get_value", object)
}

#' @export
get_value.pop_data <- function(object, ...){

  # get age data
  value_data <- object %>% pull('value')

  return(value_data)
}

get_id <- function(object, ...)
{
  UseMethod("get_id", object)
}

#' @export
get_id.pop_data <- function(object, ...){

  # get age data
  id_data <- object %>% pull('id')

  return(id_data)
}
