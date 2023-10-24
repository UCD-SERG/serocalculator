get_curve_params_one_antigen = function(params, antigen)
{
  params %>%
  filter(antigen_iso == .env[["antigen"]]) %>%
    mutate(alpha = alpha*365.25,
           d = r-1) %>%
    select(y1, alpha, d)
}
