test_that(
  "est.incidence() produces expected results for typhoid data",
  {
    library(readr)
    library(dplyr)
    c.hlye.IgG <-
      fs::path_package(
        "extdata",
        "dmcmc_hlyeigg_09.30.rds",
        package = "serocalculator") |> #Load longitudinal parameters dataset
      readRDS()%>%
      select(y1, alpha, r, antigen_iso)

    p.hlye.IgG  <-
      fs::path_package(
        package = "serocalculator",
        "extdata/simpophlyeigg.2.csv") %>% #Load simulated cross-sectional dataset
      read_csv(
        col_types = cols(
          a.smpl = col_double(),
          y.smpl = col_double(),
          i = col_double(),
          t = col_double()
        )
      ) %>%
      rename( #rename variables
        y = y.smpl,
        a = a.smpl) %>%
      select(y, a) |>
      mutate(antigen_iso = "HlyE_IgG")

    cond.hlye.IgG <- data.frame(
      nu = 1.027239,             # B noise
      eps = 0.2,            # M noise
      y.low = 0.0,          # low cutoff
      y.high = 5e4,
      antigen_iso = "HlyE_IgG");

    start <- .05

    fit = est.incidence(
      dpop = p.hlye.IgG,
      dmcmc = c.hlye.IgG,
      c.age = NULL,
      antigen_isos = "HlyE_IgG",
      noise_params = cond.hlye.IgG,
      start = start,
      print.level = 2,
      iterlim = 100)

    # compare with `typhoid_results` from data-raw/typhoid_results.qmd

    expect_equal(
      object = fit,
      expected = typhoid_results)
  })
