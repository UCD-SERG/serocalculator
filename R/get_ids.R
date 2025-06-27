#' Get person ID variable name
#'
#' @param object a [data.frame]
#' @param ... unused
#'
#' @returns a [character] string containing the person ID column,
#' as recorded in the metadata of `object`
#' @export
#' @keywords internal
#' @examples
#' ids_varname(sees_pop_data_pk_100)
ids_varname <- function(object, ...) {
  id_var <- attributes(object)$id_var

  rlang_issue <-
    inherits(object, "rlang_data_pronoun") &
    is.null(id_var)

  if (rlang_issue) {
    cli::cli_abort("can't extract attributes from pronouns")
  }

  if (is.null(id_var)) {

    if ("index_id" %in% names(object)) {
      cli::cli_warn(
        message = c(
          "No `id_var` attribute found in {.arg object}.",
          "i" = "Defaulting to 'index_id'."
        )
      )
      id_var <- "index_id"
    } else {
      cli::cli_abort(
        message = "No `id_var` attribute found in {.arg object}."
      )
    }
  }

  return(id_var)
}

# old name: unexported
get_id_var <- ids_varname

#' Get person IDs
#'
#' @param object a [data.frame()]
#' @param ... unused
#'
#' @returns a [character] [vector]
#' @export
#' @keywords internal
#' @examples
#' ids(sees_pop_data_pk_100)
ids <- function(object, ...) {
  id_var_name <- object |> ids_varname()
  id_data <- object[[id_var_name]]
  return(id_data)
}

# old name: unexported
get_id <- ids
