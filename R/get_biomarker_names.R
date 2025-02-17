get_biomarker_names <- function(object, ...) {
  # get biomarker name data
  biomarker_names_var <- get_biomarker_names_var(object)
  biomarker_data <- object |> pull(biomarker_names_var)

  return(biomarker_data)
}

