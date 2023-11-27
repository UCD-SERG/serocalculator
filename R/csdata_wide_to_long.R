csdata_wide_to_long = function(
    csdata,
    antigen_isos = c("HlyE_IgG", "HlyE_IgA"))
{
  csdata %>%
    as_tibble() %>%
    pivot_longer(
      cols = antigen_isos,
      names_to = "antigen_iso",
      values_to = "value")
}
