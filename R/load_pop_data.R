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
#'                         age = "Age",
#'                         value = "result",
#'                         id = "index_id",
#'                         standardize = TRUE)
#'
#' print(xs_data)
load_pop_data = function(file_path,
                         antigen_isos = NULL,
                         age = "Age",
                         value = "result",
                         id = "index_id",
                         standardize = TRUE)
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

  if(standardize)
  {
    pop_data = pop_data %>%
                    set_age(age = age, standardize = standardize) %>%
                    set_value(value = value, standardize = standardize) %>%
                    set_id(id = id, standardize = standardize)
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


set_age <- function(object, ...){
  UseMethod("set_age", object)
}

#' @export
set_age.pop_data <- function(object, age = 'Age', standardize, ...){

  if(standardize)
  {
    # set age attribute
    attr(object, "age_var") <- 'age'

    # rename provided column
    if(age %in% colnames(object))
    {
      object <- object %>%
        rename('age' = age)
    } else
    {
      cli::cli_abort(
        '`age = "{age}"` is not a valid input;
      the age column in not available in the data set')
    }


  } else if (!standardize)
  {
    # check if age column exists
    if(age %in% colnames(object))
    {
      attr(object, "age_var") <- age
    } else
    {
      # search age variable from object
      age_var <- object %>%
        select(tidyselect::matches("age",colnames(object),ignore.case = TRUE) & ends_with("e", ignore.case = TRUE)) %>%
        names()

      if(length(age_var) > 0)
      {
        attr(object, "age_var") <- age_var

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
  }

  return(object)
}


set_value <- function(object, ...){
  UseMethod("set_value", object)
}

#' @export
set_value.pop_data <- function(object, value = 'result', standardize, ...){

  if(standardize)
  {
    # set attribute
    attr(object, "value_var") <- 'value'

    # rename provided column
    if(value %in% colnames(object))
    {
      object <- object %>%
        rename('value' = value)
    } else
    {
      cli::cli_abort(
        '`value = "{value}"` is not a valid input;
      the value column in not available in the data set')
    }


  } else if(!standardize)
  {
    # search value variable from pop_data
    value_var <- object %>%
      select(tidyselect::matches("result", ignore.case = TRUE)) %>%
      names()

    if(length(value_var) > 0)
    {
      attr(object, "value_var") <- value_var

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
  return(object)
}

set_id <- function(object, ...){
  UseMethod("set_id", object)
}

#' @export
set_id.pop_data <- function(object, id = 'index_id', standardize, ...){

  if(standardize)
  {
    # set attribute
    attr(object, "id_var") <- 'id'

    # rename provided column
    if(id %in% colnames(object))
    {
      object <- object %>%
        rename('id' = id)

    } else
    {
      cli::cli_abort(
        '`id = "{id}"` is not a valid input;
      the id column in not available in the data set')
    }

  } else if (!standardize)
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

  return(object)
}

