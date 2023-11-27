prep_curve_params_for_array = function(data)
{
  data |>
    mutate(y1 = .data$y1 - .data$y0) %>%
    mutate(r = .data$r - 1) %>%
    tidyr::pivot_longer(
      names_to = "parameter",
      cols = c("y0", "y1", "t1", "alpha", "r") ) |>
    mutate(
      parameter =
        .data$parameter |>
        factor(
          levels = unique(.data$parameter))) %>%
    select(-any_of(c("Country", "ageCat"))) %>%
    mutate(value = log(.data$value)) %>%
    droplevels()
}
