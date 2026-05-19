noise_params_test0 <- function(ymax = 0.5, yval = 0.6) {

  cur_antibody <- "HlyE_IgA"

  # load in longitudinal parameters
  curve_params <-
    serocalculator::typhoid_curves_nostrat_100 |>
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

  xs_data <- serocalculator::sees_pop_data_pk_100 |>
    dplyr::filter(
      .data$antigen_iso == cur_antibody
    ) |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(value = yval)

  # Load noise params
  noise_params <- tibble(
    antigen_iso = cur_antibody,
    nu = 0.5, # Biologic noise (nu)
    eps = 0, # M noise (eps)
    y.low = 0, # low cutoff (llod)
    y.high = ymax # high cutoff (y.high)
  )

  lambda <- 0.1
  f_dev0(
    lambda = lambda,
    csdata = xs_data,
    lnpars = curve_params,
    cond = noise_params
  )

}

noise_params_test <- Vectorize(noise_params_test0, c("ymax", "yval"))
