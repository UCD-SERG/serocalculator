#' Load a cross-sectional antibody survey data set
#'
#' @param file_path path to an RDS file containing a cross-sectional antibody survey data set, stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector of antigen isotypes to be used in analyses
#' @param age a [character()] identifying the age column
#' @param id a [character()] identifying the id column
#' @param value a [character()] identifying the value column
#' @param standardize a [logical()] to determine standardization of columns
#' @returns a `pop_data` object (a [tibble::tbl_df] with extra attribute `antigen_isos`)
#' @export
#' @examples
#' xs_data <- load_pop_data("https://osf.io/download//n6cp3/")
#'
#' print(xs_data)
load_pop_data <- function(file_path,
                          antigen_isos = NULL,
                          age = "Age",
                          value = "result",
                          id = "index_id",
                          standardize = TRUE) {
  if (file_path %>% substr(1, 4) == "http") {
    file_path <- url(file_path)
  }

  pop_data <-
    file_path %>%
    readRDS() %>%
    tibble::as_tibble()

  class(pop_data) <-
    c("pop_data", class(pop_data))

  if (is.null(antigen_isos)) {
    antigen_isos <- unique(pop_data$antigen_iso)
  } else {
    stopifnot(all(is.element(antigen_isos, pop_data$antigen_iso)))
  }

  attr(pop_data, "antigen_isos") <- antigen_isos

  pop_data <- pop_data %>%
    set_age(age = age, standardize = standardize) %>%
    set_value(value = value, standardize = standardize) %>%
    set_id(id = id, standardize = standardize)

  return(pop_data)
}

get_age <- function(object, ...) {
  UseMethod("get_age", object)
}

get_age.pop_data <- function(object, ...) {
  # get age data
  age_data <- object %>% pull(attr(object, "age_var"))

  return(age_data)
}

get_age_var <- function(object, ...) {
  UseMethod("get_age_var", object)
}

get_age_var.pop_data <- function(object, ...) {
  # get value attribute
  age_var <- attributes(object)$age_var

  return(age_var)
}

get_value <- function(object, ...) {
  UseMethod("get_value", object)
}

get_value.pop_data <- function(object, ...) {
  # get age data
  value_data <- object %>% pull(attr(object, "value_var"))

  return(value_data)
}

get_value_var <- function(object, ...) {
  UseMethod("get_value_var", object)
}

get_value_var.pop_data <- function(object, ...) {
  # get value attribute
  value_var <- attributes(object)$value_var

  return(value_var)
}

get_id <- function(object, ...) {
  UseMethod("get_id", object)
}

get_id.pop_data <- function(object, ...) {
  # get age data
  id_data <- object %>% pull(attr(object, "id_var"))

  return(id_data)
}

get_id_var <- function(object, ...) {
  UseMethod("get_id_var", object)
}

get_id_var.pop_data <- function(object, ...) {
  # get value attribute
  id_var <- attributes(object)$id_var

  return(id_var)
}

set_age <- function(object, ...) {
  UseMethod("set_age", object)
}

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
