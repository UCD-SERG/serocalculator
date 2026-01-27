\dontrun{
library(dplyr)

xs_data <- sees_pop_data_pk_100

curve <-
  typhoid_curves_nostrat_100 |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

noise <- example_noise_params_pk

# Fit model with clustering
est_cluster <- est_seroincidence(
  pop_data = xs_data,
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  cluster_var = "cluster"
)

# Calculate ICC
icc_result <- compute_icc(est_cluster)
print(icc_result$icc)

# With stratified analysis
est_by_cluster <- est_seroincidence_by(
  pop_data = xs_data,
  strata = "catchment",
  sr_params = curve,
  noise_params = noise,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  cluster_var = "cluster"
)

# Calculate ICC for each stratum
icc_by_result <- compute_icc(est_by_cluster)
print(icc_by_result)
}
