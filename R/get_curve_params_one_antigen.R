get_curve_params_one_antigen = function(params, antigen)
{
  params %>%
  dplyr::filter(.data[["antigen_iso"]] == .env[["antigen"]]) %>%
    mutate(
      alpha = .data[["alpha"]] * 365.25,
      d = .data[["r"]] - 1) %>%
    select("y1", "alpha", "d")
}
