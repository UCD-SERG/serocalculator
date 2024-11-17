#' Load noise parameters
#'
#' @param file_path path to an RDS file containing biologic
#' and measurement noise of antibody decay curve parameters
#' `y.low`, `eps`, `nu`, and `y.high`,
#' stored as a [data.frame()] or [tibble::tbl_df]
#' @param antigen_isos [character()] vector
#' of antigen isotypes to be used in analyses
#'
#' @returns a `noise` object (a [tibble::tbl_df]
#' with extra attribute `antigen_isos`)
#' @export
#' @examples
#' noise <- load_noise_params("https://osf.io/download//hqy4v/")
#' print(noise)
#'
load_noise_params <- function(file_path, antigen_isos = NULL) {
  if (file_path |> substr(1, 4) == "http") {
    file_path <- url(file_path)
  }

  noise <-
    file_path |>
    readRDS() |>
    as_noise_params()

  return(noise)
}
