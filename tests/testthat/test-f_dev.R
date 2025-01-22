test_that("`f_dev0()` and `f_dev()` produce stable results", {

  library(dplyr)
  library(tibble)

  # load in longitudinal parameters
  curve_params = load_curve_params(serocalculator_example("example_curve_params.rds"))
  xs_data <- load_pop_data(serocalculator_example("example_pop_data.rds"))

  #Load noise params
  noise_params <- tibble(
    antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
    nu = c(0.5, 0.5),                          # Biologic noise (nu)
    eps = c(0, 0),                             # M noise (eps)
    y.low = c(1, 1),                           # low cutoff (llod)
    y.high = c(5e6, 5e6))                      # high cutoff (y.high)

  cur_antibody = "HlyE_IgA"

  cur_data =
    xs_data %>%
    dplyr::filter(
     .data$catchment == "dhaka",
     .data$antigen_iso == cur_antibody) %>%
    slice_head(n = 100)

  cur_curve_params =
    curve_params %>%
    dplyr::filter(.data$antigen_iso == cur_antibody)  %>%
    slice_head(n = 100)

  cur_noise_params =
    noise_params %>%
    dplyr::filter(.data$antigen_iso == cur_antibody)

  if(!is.element('d', names(cur_curve_params)))
  {
    cur_curve_params =
      cur_curve_params %>%
      dplyr::mutate(
        alpha = .data$alpha * 365.25,
        d = .data$r - 1)
  }

  lambda = 0.1
  expect_snapshot_value(
    x = f_dev0(
      lambda = lambda,
      csdata = cur_data,
      lnpars = cur_curve_params,
      cond = cur_noise_params
    ),
    style = "deparse")

  lambdas = seq(0.1, 0.2, by = .01)
  expect_snapshot_value(
    x = f_dev(
      lambda = lambdas,
      csdata = cur_data,
      lnpars = cur_curve_params,
      cond = cur_noise_params
    ),
    style = "deparse")

})
