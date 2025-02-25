test_that("`f_dev0()` and `f_dev()` produce stable results", {
  withr::local_package("dplyr")
  withr::local_package("tibble")

  # load in longitudinal parameters
  curve_params <- typhoid_curves_nostrat_100
  xs_data <- sees_pop_data_pk_100

  # Load noise params
  noise_params <- tibble(
    antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
    nu = c(0.5, 0.5), # Biologic noise (nu)
    eps = c(0, 0), # M noise (eps)
    y.low = c(1, 1), # low cutoff (llod)
    y.high = c(5e6, 5e6) # high cutoff (y.high)
  )

  cur_antibody <- "HlyE_IgA"

  cur_data <-
    xs_data |>
    dplyr::filter(
      .data$antigen_iso == cur_antibody
    ) |>
    dplyr::slice_head(n = 100)

  cur_curve_params <-
    curve_params |>
    dplyr::filter(.data$antigen_iso == cur_antibody) |>
    dplyr::slice_head(n = 100)

  cur_noise_params <-
    noise_params |>
    dplyr::filter(.data$antigen_iso == cur_antibody)

  if (!is.element("d", names(cur_curve_params))) {
    cur_curve_params <-
      cur_curve_params |>
      dplyr::mutate(
        alpha = .data$alpha * 365.25,
        d = .data$r - 1
      )
  }

  lambda <- 0.1
  expect_snapshot_value(
    x = f_dev0(
      lambda = lambda,
      csdata = cur_data,
      lnpars = cur_curve_params,
      cond = cur_noise_params
    ),
    style = "deparse"
  )

  lambdas <- seq(0.1, 0.2, by = .01)
  expect_snapshot_value(
    x = f_dev(
      lambda = lambdas,
      csdata = cur_data,
      lnpars = cur_curve_params,
      cond = cur_noise_params
    ),
    style = "deparse"
  )
})

test_that(
  desc = "`f_dev()` handles censored data",
  code = {
    withr::local_package("dplyr")
    withr::local_package("tibble")

    cur_antibody <- "HlyE_IgA"

    # load in longitudinal parameters
    curve_params <-
      typhoid_curves_nostrat_100 |>
      dplyr::filter(.data$antigen_iso == cur_antibody) |>
      dplyr::slice_head(n = 100)

    if (!is.element("d", names(curve_params))) {
      curve_params <-
        curve_params |>
        dplyr::mutate(
          alpha = .data$alpha * 365.25,
          d = .data$r - 1
        )
    }

    xs_data <- sees_pop_data_pk_100 |>
      dplyr::filter(
        .data$antigen_iso == cur_antibody
      ) |>
      dplyr::slice_head(n = 1) |>
      dplyr::mutate(value = .6)

    # Load noise params
    noise_params <- tibble(
      antigen_iso = "HlyE_IgA",
      nu = 0.5, # Biologic noise (nu)
      eps = 0, # M noise (eps)
      y.low = 0, # low cutoff (llod)
      y.high = .5 # high cutoff (y.high)
    )


    lambda <- 0.1
    expect_snapshot_value(
      x = f_dev0(
        lambda = lambda,
        csdata = xs_data,
        lnpars = curve_params,
        cond = noise_params
      ),
      style = "deparse"
    )

    c(.1, .2, .5, .6, .61, 10, 20) |>
      noise_params_test(ymax = _, yval = 0.6) |>
      expect_snapshot_value(style = "deparse")

    c(.1, .2, .5, .6, .61, 10, 20) |>
      noise_params_test(ymax = 0.6, yval = _) |>
      expect_snapshot_value(style = "deparse")

  }
)

