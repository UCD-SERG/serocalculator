prep_curve_params_for_array = function(data)
{
  data |>
    mutate(y1 = .data$y1 - .data$y0) %>%
    mutate(d = .data$r - 1) %>% # DM: issue here?
    tidyr::pivot_longer(
      names_to = "parameter",
      cols = c("y0", "y1", "t1", "alpha", "d") ) |>
    mutate(
      parameter =
        .data$parameter |>
        factor(
          levels = c("y0", "y1", "t1", "alpha", "d"))) %>%
    select(-any_of(c("Country", "ageCat"))) %>%
    mutate(value = log(.data$value)) %>%
    droplevels()
}
