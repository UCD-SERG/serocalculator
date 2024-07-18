typhoid_noise_params <-
  tibble::tribble(
    ~Country,      ~antigen_iso, ~llod,    ~nu,      ~y.high, ~eps,
    "MGH",         "HlyE_IgA",   0.17922,  2.86983,  5e+06,   0.23961,
    "bangladesh",  "HlyE_IgA",   0.17922,  2.86983,  5e+06,   0.2805,
    "nepal",       "HlyE_IgA",   0.17922,  2.86983,  5e+06,   0.23835,
    "pakistan",    "HlyE_IgA",   0.17922,  2.86983,  5e+06,   0.27932,
    "MGH",         "HlyE_IgG",   0.645,    3.0252,   5e+06,   0.16392,
    "bangladesh",  "HlyE_IgG",   0.645,    3.0252,   5e+06,   0.30608,
    "nepal",       "HlyE_IgG",   0.645,    3.0252,   5e+06,   0.12821,
    "pakistan",    "HlyE_IgG",   0.645,    3.0252,   5e+06,   0.14564
  )

usethis::use_data(typhoid_noise_params, overwrite = TRUE)
