test_that(
  desc = "scatterplot results are consistent",
  code = {

    withr::local_package("dplyr")
    withr::local_package("ggplot2")

    xs_data <-
      sees_pop_data_pk_100

    curve <-
      typhoid_curves_nostrat_100 |>
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

    noise <-
      example_noise_params_pk

    est2 <- est_seroincidence_by(
      strata = c("catchment", "ageCat"),
      pop_data = xs_data,
      sr_params = curve,
      noise_params = noise,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2 # Allow for parallel processing to decrease run time
    )

    est2sum <- summary(est2) |>
      mutate(catchment = catchment |>
               labelled::set_label_attribute("Catchment Area"))

    plot1 <- autoplot(est2sum,
                      xvar = "ageCat",
                      type = "scatter",
                      dodge_width = 0.1,
                      color_var = "catchment",
                      CI = TRUE)

    plot1 |> vdiffr::expect_doppelganger(title = "strat-est-plot-CI")

    plot2 <- autoplot(est2sum,
                      xvar = "ageCat",
                      type = "scatter",
                      CI = TRUE,
                      dodge_width = 0.1,
                      group_var = "catchment",
                      color_var = "catchment")

    plot2 |> vdiffr::expect_doppelganger(title = "strat-est-plot-CI-lines")


  }
)

test_that(
  desc = "barplot results are consistent",
  code = {

    withr::local_package("dplyr")
    withr::local_package("ggplot2")

    xs_data <-
      sees_pop_data_pk_100

    curve <-
      typhoid_curves_nostrat_100 |>
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

    noise <-
      example_noise_params_pk

    est2 <- est_seroincidence_by(
      strata = c("catchment", "ageCat"),
      pop_data = xs_data,
      sr_params = curve,
      noise_params = noise,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2 # Allow for parallel processing to decrease run time
    )

    est2sum <- summary(est2)

    plot1 <- autoplot(est2sum,
                      yvar = "ageCat",
                      type = "bar",
                      dodge_width = 0.1,
                      color_var = "catchment",
                      CI = TRUE)

    plot1 |> vdiffr::expect_doppelganger(title = "strat-est-barplot")
  }
)

test_that(
  desc = "error on plot type",
  code = {

    withr::local_package("dplyr")
    withr::local_package("ggplot2")

    xs_data <-
      sees_pop_data_pk_100

    curve <-
      typhoid_curves_nostrat_100 |>
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

    noise <-
      example_noise_params_pk

    est2 <- est_seroincidence_by(
      strata = c("catchment", "ageCat"),
      pop_data = xs_data,
      sr_params = curve,
      noise_params = noise,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2 # Allow for parallel processing to decrease run time
    )

    est2sum <- summary(est2)

    expect_snapshot(
      error = TRUE,
      x = {
        plot1 <- autoplot(est2sum,
                          xvar = "ageCat",
                          type = "whisker", #invalid plot type
                          dodge_width = 0.1,
                          color_var = "catchment",
                          CI = TRUE)
      }
    )
  }
)

test_that(
  desc = "error on incorrect yvar",
  code = {
    withr::local_package("dplyr")
    withr::local_package("ggplot2")

    xs_data <-
      sees_pop_data_pk_100

    curve <-
      typhoid_curves_nostrat_100 |>
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

    noise <-
      example_noise_params_pk

    est2 <- est_seroincidence_by(
      strata = c("catchment", "ageCat"),
      pop_data = xs_data,
      sr_params = curve,
      noise_params = noise,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2 # Allow for parallel processing to decrease run time
    )

    est2sum <- summary(est2)

    expect_snapshot(
      error = TRUE,
      x = {
        plot1 <- autoplot(est2sum,
                          yvar = "fake", #invalid variable name
                          type = "bar",
                          dodge_width = 0.1,
                          color_var = "catchment",
                          CI = TRUE)

      }
    )
  }
)

test_that(
  desc = "color palette works as expected",
  code = {
    withr::local_package("dplyr")
    withr::local_package("ggplot2")

    xs_data <-
      sees_pop_data_pk_100

    curve <-
      typhoid_curves_nostrat_100 |>
      filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

    noise <-
      example_noise_params_pk

    est2 <- est_seroincidence_by(
      strata = c("catchment", "ageCat"),
      pop_data = xs_data,
      sr_params = curve,
      noise_params = noise,
      curve_strata_varnames = NULL,
      noise_strata_varnames = NULL,
      antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
      num_cores = 2 # Allow for parallel processing to decrease run time
    )

    est2sum <- summary(est2)

    new_color_palette <- c("#EA6552", "#8F4B86", "#0099B4FF")

    plot1 <- autoplot(est2sum,
                      yvar = "ageCat",
                      type = "bar",
                      dodge_width = 0.1,
                      color_var = "catchment",
                      color_palette = new_color_palette, #manual color palette
                      CI = TRUE)

    plot1 |> vdiffr::expect_doppelganger(title = "strat-est-barplot-palette")
  }
)
