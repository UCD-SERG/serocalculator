get_age_var <- function(object, ...) {
  age_var <- attributes(object)$age_var
  return(age_var)
}

get_age <- function(object, ...) {
  age_var <- object |> get_age_var()
  age_data <- object |> pull(age_var)
  return(age_data)
}

#' Extract antibody measurement values
#'
#' @param object a `pop_data` object
#' @param ... unused
#'
#' @returns the name of the column in `object` specified as containing
#' antibody abundance measurements
#' @export
#'
#' @examples
#' sees_pop_data_100 |> get_values_var()
get_values_var <- function(object, ...) {
  value_var <- attributes(object)$value_var
  return(value_var)
}

get_value_var <- get_values_var

#' Get antibody measurement values
#'
#' @param object a `pop_data` object
#' @param ... unused
#'
#' @returns a [numeric] [vector] of antibody measurement values
#' @export
#'
#' @examples
#' sees_pop_data_100 |> get_values()
get_values <- function(object, ...) {
  value_var_name <- object |> get_values_var()
  value_data <- object |> pull(value_var_name)
  return(value_data)
}

# old name: unexported
get_value <- get_values

#' Extract biomarker levels
#'
#' @param object a `pop_data` object
#' @param ... unused
#'
#' @returns the biomarker levels in `object`
#' @export
#'
#' @examples
#' sees_pop_data_100 |> get_biomarker_levels()
get_biomarker_levels <- function(object, ...) {
  metadata <- attributes(object)
  if ("antigen_isos" %in% names(metadata)) {
    return(metadata[["antigen_isos"]])
  } else if (is.data.frame(object) && "antigen_isos" %in% names(object)) {
    return(unique(object[["antigen_isos"]]))
  } else {
    cli::cli_abort("biomarkers not found in `object`")
  }
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
    object <- object |>
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
    object <- object |>
      rename(c("value" = attr(object, "value_var")))

    # set id attribute
    attr(object, "value_var") <- "value"
  }

  return(object)
}
