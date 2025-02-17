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
    object <- object |>
      rename(c("id" = attr(object, "id_var")))

    # set id attribute
    attr(object, "id_var") <- "id"
  }

  return(object)
}
