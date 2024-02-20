#' Extract antigen-isotype combinations vector from an object
#'
#' @param object any R object
#'
#' @return a [character()] vector of the antigen isotype combinations associated with that object, if any exist
#' @export
#'
#' @examples
#'
#' "https://osf.io/download//n6cp3/" |>
#'  load_pop_data() |>
#'  clean_pop_data() |>
#'  get_antigen_isos()
get_antigen_isos = function(object)
{
  if("antigen_isos" %in% names(attributes(object)))
  {
    to_return = object |> attr("antigen_isos")
  } else if("antigen_isos" %in% names(object))
  {
    to_return = object$"antigen_isos" |> unique()
  } else
  {
    stop('"antigen_isos" not found in object.')
  }
  return(to_return |> as.character())
}
