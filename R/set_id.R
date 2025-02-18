#' Specify person ID column
#' @description
#' Sets the `id_var` metadata attribute of `object`
#' @param object a [data.frame]
#' @param id [character] string at least partially matching a column
#' in `object`
#' @param standardize whether to rename the column specified by `id`
#' to "id"
#' @param ... unused
#'
#' @returns a modified version of `object`
#' @export
#' @keywords internal
#'
#' @examples
#' serocalculator_example("example_pop_data.rds") |>
#'   readr::read_rds() |>
#'     set_id(id = "index_id") |>
#'     attr("id_var")
set_id <- function(object,
                   id = "index_id",
                   standardize = TRUE,
                   ...) {
  # check if id column exists
  if (id %in% colnames(object)) {
    attr(object, "id_var") <- id
  } else {

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
      cli::cli_warn(
        message = c(
          "The specified {.var id} column {.val {id}} does not exist.",
          "i" = 'Proceeding to use "{id_var}"'
        )
      )
    } else if (length(id_var) == 0) {
      cli::cli_abort(
        message = c(
          "The specified {.var id} column {.val {id}} does not exist.",
          "x" = "No similar column name was detected."
        )
      )
    } else {
      # if (length(id_var) > 1)
      cli::cli_warn(
        message = c(
          "The specified {.var id} column {.val {id}} does not exist.",
          "i" = "Multiple potential matches found: {.var {id_var}}",
          "i" = "Using first match: {.var {id_var[1]}}"
        )
      )

      attr(object, "id_var") <- id_var[1]
    }
  }

  if (standardize) {
    object <- object |>
      rename(c("id" = attr(object, "id_var")))

    # set id attribute
    attr(object, "id_var") <- "id"
  }

  return(object)
}
