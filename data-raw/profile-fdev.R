load_all()

library(dplyr)
library(tibble)

# load in longitudinal parameters
curve_params = load_sr_params("https://osf.io/download/rtw5k")
xs_data <- "https://osf.io/download//n6cp3/" %>% load_pop_data()

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
    .data$antigen_iso == cur_antibody)

cur_curve_params =
  curve_params %>%
  dplyr::filter(.data$antigen_iso == cur_antibody)

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
lambdas = seq(0.1, 0.2, by = .01)

system.time(
  {
    results = numeric(length(lambdas))
    for (i in 1:length(lambdas))
    {
      results[i] = f_dev0(
        lambda = lambdas[i],
        csdata = cur_data,
        lnpars = cur_curve_params,
        cond = cur_noise_params
      )
    }

  })

system.time(
  {
    results2 = f_dev(
      lambda = lambdas,
      csdata = cur_data,
      lnpars = cur_curve_params,
      cond = cur_noise_params
    )}
)
