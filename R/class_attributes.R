get_age_var <- function(object, ...) {
  age_var <- attributes(object)$age_var
  return(age_var)
}

get_age <- function(object, ...) {
  age_var <- object %>% get_age_var()
  age_data <- object %>% pull(age_var)
  return(age_data)
}

get_value_var <- function(object, ...) {
  value_var <- attributes(object)$value_var
  return(value_var)
}

get_value <- function(object, ...) {
  value_var_name <- object %>% get_value_var()
  value_data <- object %>% pull(value_var_name)
  return(value_data)
}

get_id_var <- function(object, ...) {
  id_var <- attributes(object)$id_var
  return(id_var)
}

get_id <- function(object, ...) {
  id_var_name <- object %>% get_id_var()
  id_data <- object %>% pull(id_var_name)
  return(id_data)
}

get_biomarker_levels <- function(object, ...) {
  attr(object, "antigen_isos")
}

get_biomarker_names_var <- function(object, ...) {
  # get value attribute
  biomarker_var <- attributes(object)[["biomarker_var"]]

  return(biomarker_var)
}

get_biomarker_names <- function(object, ...) {
  # get biomarker name data
  biomarker_names_var <- get_biomarker_names_var(object)
  biomarker_data <- object %>% pull(biomarker_names_var)

  return(biomarker_data)
}


set_age <- function(object,
                    age = "Age",
                    standardize = TRUE,
                    ...) {
  # check if age column exists
  if (age %in% colnames(object)) {
    attr(object, "age_var") <- age
  } else {
    cli::cli_warn(class = "missing variable",
                  'The specified `age` column "{age}" does not exist.')

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
      cli::cli_inform('Proceeding to use "{.var {age_var}}"')
    } else if (length(age_var) == 0) {
      cli::cli_abort("No similar column name was detected.")
    } else if (length(age_var) > 1) {
      cli::cli_warn("Multiple potential matches found: {.var {age_var}}")
      cli::cli_warn("Using first match: {.var {age_var[1]}}")
      attr(object, "age_var") <- age_var[1]
    } else {
      cli::cli_abort("{.code length(age_var)} = {.val {length(age_var)}}")
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


set_value <- function(object,
                      value = "result",
                      standardize = TRUE,
                      ...) {
  # check if value column exists
  if (value %in% colnames(object)) {
    attr(object, "value_var") <- value
  } else {
    cli::cli_warn('The specified `value` column "{.var {value}}"
                  does not exist.')

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
      cli::cli_inform('Proceeding to use "{.var {value_var}}"')
    } else if (length(value_var) == 0) {
      cli::cli_abort("No similar column name was detected.")
    } else {
      # i.e. if (length(value_var) > 1)
      cli::cli_warn("Multiple potential matches found: {.var {value_var}}")
      cli::cli_inform("Using first match: {.var {value_var[1]}}")
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

set_id <- function(object,
                   id = "index_id",
                   standardize = TRUE,
                   ...) {
  # check if id column exists
  if (id %in% colnames(object)) {
    attr(object, "id_var") <- id
  } else {
    cli::cli_warn("The specified {.var id} column {.val {id}} does not exist.")

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
      cli::cli_inform('Proceeding to use "{id_var}"')
    } else if (length(id_var) == 0) {
      cli::cli_abort("No similar column name was detected.")
    } else {
      # if (length(id_var) > 1)
      cli::cli_warn("Multiple potential matches found: {.var {id_var}}")
      cli::cli_inform("Using first match: {.var {id_var[1]}}")
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
