#' Get person ID variable name
#'
#' @param object a [data.frame]
#' @param ... unused
#'
#' @returns a [character] string containing the person ID column,
#' as recorded in the metadata of `object`
#' @export
#'
#' @examples
#' ids_varname(sees_pop_data_pk_100)
ids_varname <- function(object, ...) {
  id_var <- attributes(object)$id_var
  if (is.null(id_var)) {
    cli::cli_warn(
      message = c(
        "No `id_var` attribute found in {.arg object}.",
        "i" = "Defaulting to 'index_id'."
      )
    )
    id_var <- "index_id"
  }

  return(id_var)
}

#' Get person IDs
#'
#' @param object a [data.frame()]
#' @param ... unused
#'
#' @returns a [character] [vector]
#' @export
#'
#' @examples
#' ids(sees_pop_data_pk_100)
ids <- function(object, ...) {
  id_var_name <- object |> ids_varname()
  id_data <- object |> pull(id_var_name)
  return(id_data)
}
