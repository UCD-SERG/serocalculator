get_xspd_one_antigen = function(dpop, antigen)
{

  dpop %>%
    dplyr::filter(.data$antigen_iso %in% .env$antigen) %>%
    select("y", "a") %>%
    drop_na()

}
