get_id_var <- function(object, ...) {
  id_var <- attributes(object)$id_var
  return(id_var)
}

get_id <- function(object, ...) {
  id_var_name <- object |> get_id_var()
  id_data <- object |> pull(id_var_name)
  return(id_data)
}
