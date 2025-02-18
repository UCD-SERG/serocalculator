get_person_id_varname <- function(object, ...) {
  id_var <- attributes(object)$id_var
  return(id_var)
}

get_person_ids <- function(object, ...) {
  id_var_name <- object |> get_person_id_varname()
  id_data <- object |> pull(id_var_name)
  return(id_data)
}
