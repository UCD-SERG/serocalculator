set_biomarker_var <- function(object,
                              biomarker = "antigen_iso",
                              standardize = TRUE,
                              ...) {
  if (biomarker %in% colnames(object)) {
    attr(object, "biomarker_var") <- biomarker
  } else {
    cli::cli_abort('data does not include column "{biomarker}"',
                   class = "missing variable")
  }

  if (standardize) {
    object <- object |>
      rename(c("antigen_iso" = attr(object, "biomarker_var")))

    # update attribute
    attr(object, "biomarker_var") <- "antigen_iso"
  }

  return(object)

}
