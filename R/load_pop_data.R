#' Load a cross-sectional antibody survey data set
#'
#' @param file_path path to an RDS file containing a cross-sectional antibody survey data set, stored as a [data.frame()] or [tibble::tbl_df]
#' @inheritDotParams as_pop_data
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attributes)
#' @export
#' @examples
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#' print(xs_data)
load_pop_data <- function(file_path,
                          ...) {

  if (file_path %>% substr(1, 4) == "http") {
    file_path <- url(file_path)
  }

  pop_data <-
    file_path %>%
    readRDS() %>%
    as_pop_data(...)

  return(pop_data)
}

get_age <- function(object, ...) {
  UseMethod("get_age", object)
}

#' @export
get_age.pop_data <- function(object, ...) {
  # get age data
  age_data <- object %>% pull(attr(object, "age_var"))

  return(age_data)
}

get_age_var <- function(object, ...) {
  UseMethod("get_age_var", object)
}

#' @export
get_age_var.pop_data <- function(object, ...) {
  # get value attribute
  age_var <- attributes(object)$age_var

  return(age_var)
}

get_value <- function(object, ...) {
  UseMethod("get_value", object)
}

#' @export
get_value.pop_data <- function(object, ...) {
  # get age data
  value_data <- object %>% pull(attr(object, "value_var"))

  return(value_data)
}

get_value_var <- function(object, ...) {
  UseMethod("get_value_var", object)
}

#' @export
get_value_var.pop_data <- function(object, ...) {
  # get value attribute
  value_var <- attributes(object)$value_var

  return(value_var)
}

get_id <- function(object, ...) {
  UseMethod("get_id", object)
}

#' @export
get_id.pop_data <- function(object, ...) {
  # get age data
  id_data <- object %>% pull(attr(object, "id_var"))

  return(id_data)
}

get_id_var <- function(object, ...) {
  UseMethod("get_id_var", object)
}

#' @export
get_id_var.pop_data <- function(object, ...) {
  # get value attribute
  id_var <- attributes(object)$id_var

  return(id_var)
}

set_age <- function(object, ...) {
  UseMethod("set_age", object)
}

set_biomarker_var <- function(object, ...) {
  UseMethod("set_biomarker_var", object)
}

#' @export
set_biomarker_var.pop_data = function(object,
                                  biomarker = "antigen_iso",
                                  standardize = TRUE,
                                  ...)
{
  if (biomarker %in% colnames(object))
  {
    attr(object, "biomarker_var") <- biomarker
  } else
  {
    cli::cli_abort('data does not include column "{biomarker}"')
  }

  if (standardize)
  {
    object <- object %>%
      rename(c("antigen_iso" = attr(object, "biomarker_var")))

    # update attribute
    attr(object, "biomarker_var") <- "antigen_iso"
  }

  return(object)

}

get_biomarker_names <- function(object, ...) {
  UseMethod("get_biomarker_names", object)
}

#' @export
get_biomarker_names.pop_data <- function(object, ...) {
  # get biomarker name data
  biomarker_data <- object %>% pull(get_biomarker_names_var(object))

  return(biomarker_data)
}

get_biomarker_names_var <- function(object, ...) {
  UseMethod("get_biomarker_names_var", object)
}

#' @export
get_biomarker_names_var.pop_data <- function(object, ...) {
  # get value attribute
  biomarker_var <- attributes(object)[["biomarker_var"]]

  return(biomarker_var)
}

#' @export
set_age.pop_data <- function(object, age = "Age", standardize = TRUE, ...) {
  # check if age column exists
  if (age %in% colnames(object)) {
    attr(object, "age_var") <- age
  } else {
    cli::cli_alert_warning('The specified `age` column "{age}" does not exist.')

    # search age variable from object
    age_var <-
      grep(
        x = colnames(object),
        value = TRUE,
        pattern = age,
        ignore.case = TRUE
      )

    if (length(age_var) == 1) {
      attr(object, "age_var") <- age_var

      # create warning when using searched age instead of provided age
      cli::cli_alert_info('Proceeding to use "{age_var}"')
    } else if (length(age_var) == 0) {
      cli::cli_abort("No similar column name was detected.")
    } else # if (length(age_var) > 1)
    {
      cli::cli_alert_warning("Multiple potential matches found: {age_var}")
      cli::cli_alert_warning("Using first match: {age_var[1]}")
      attr(object, "age_var") <- age_var[1]
    }
  }

  if (standardize) {
    object <- object %>%
      rename(c("age" = attr(object, "age_var")))

    # set age attribute
    attr(object, "age_var") <- "age"
  }

  return(object)
}


set_value <- function(object, ...) {
  UseMethod("set_value", object)
}

#' @export
set_value.pop_data <- function(object, value = "result", standardize = TRUE, ...) {
  # check if value column exists
  if (value %in% colnames(object)) {
    attr(object, "value_var") <- value
  } else {
    cli::cli_alert_warning('The specified `value` column "{value}" does not exist.')

    # search value variable from pop_data
    value_var <-
      grep(
        x = colnames(object),
        value = TRUE,
        pattern = value,
        ignore.case = TRUE
      )

    if (length(value_var) == 1) {
      attr(object, "value_var") <- value_var

      # create warning when using searched age instead of provided age
      cli::cli_alert_info('Proceeding to use "{value_var}"')
    } else if (length(value_var) == 0) {
      cli::cli_abort("No similar column name was detected.")
    } else # if (length(value_var) > 1)
    {
      cli::cli_alert_warning("Multiple potential matches found: {value_var}")
      cli::cli_alert_warning("Using first match: {value_var[1]}")
      attr(object, "value_var") <- value_var[1]
    }
  }

  if (standardize) {
    object <- object %>%
      rename(c("value" = attr(object, "value_var")))

    # set id attribute
    attr(object, "value_var") <- "value"
  }

  return(object)
}

set_id <- function(object, ...) {
  UseMethod("set_id", object)
}

#' @export
set_id.pop_data <- function(object, id = "index_id", standardize = TRUE, ...) {
  # check if id column exists
  if (id %in% colnames(object)) {
    attr(object, "id_var") <- id
  } else {
    cli::cli_alert_warning('The specified `id` column "{id}" does not exist.')

    # search id variable from object
    id_var <-
      grep(
        x = colnames(object),
        value = TRUE,
        pattern = id,
        ignore.case = TRUE
      )

    if (length(id_var) == 1) {
      attr(object, "id_var") <- id_var

      # create warning when using searched id instead of provided id
      cli::cli_alert_info('Proceeding to use "{id_var}"')
    } else if (length(id_var) == 0) {
      cli::cli_abort("No similar column name was detected.")
    } else # if (length(id_var) > 1)
    {
      cli::cli_alert_warning("Multiple potential matches found: {id_var}")
      cli::cli_alert_warning("Using first match: {id_var[1]}")
      attr(object, "id_var") <- id_var[1]
    }
  }

  if (standardize) {
    object <- object %>%
      rename(c("id" = attr(object, "id_var")))

    # set id attribute
    attr(object, "id_var") <- "id"
  }

  return(object)
}
