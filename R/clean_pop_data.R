#' @title Reformat a cross-sectional antibody survey dataset
#'
#' @param pop_data a [data.frame()] containing the following columns:
#' * `index_id`: a [character()] variable identifying multiple rows of data from the same person
#' * `antigen_isos`: a [character()] variable indicating the antigen-isotype measured
#' * `result`: the measured antibody concentration
#' * `Age`: age of the individual whose serum has been assayed, at the time of blood sample collection.
#'
#' @returns A [data.frame] (or [tibble::tbl_df]) containing the following columns:
#' * `id`: a [character()] variable identifying multiple rows of data from the same person
#' * `antigen_isos`: a [character()] variable indicating the antigen-isotype measured
#' * `value`: the measured antibody concentration
#' * `age`: age of the individual whose serum has been assayed, at the time of blood sample
#' @export
#'
clean_pop_data = function(pop_data)
{
  pop_data = pop_data %>%
    rename(
      id = .data$index_id,
      value = .data$result,
      age = .data$Age)
}
