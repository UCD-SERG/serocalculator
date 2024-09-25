get_xspd_one_antigen <- function(data, antigen) {
  data %>%
    dplyr::filter(.data$antigen_iso %in% .env$antigen) %>%
    select("value", "age") %>%
    drop_na()
}
