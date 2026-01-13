library(dplyr)

# Example 1: Compare two single seroincidence estimates
# Create estimates for two different catchments
xs_data_c1 <- sees_pop_data_pk_100 |> filter(catchment == "kgh")
xs_data_c2 <- sees_pop_data_pk_100 |> filter(catchment == "aku")

est_c1 <- est_seroincidence(
  pop_data = xs_data_c1,
  sr_params = typhoid_curves_nostrat_100,
  noise_params = example_noise_params_pk,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

est_c2 <- est_seroincidence(
  pop_data = xs_data_c2,
  sr_params = typhoid_curves_nostrat_100,
  noise_params = example_noise_params_pk,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

# Compare the two estimates - returns htest format
comparison <- compare_seroincidence(est_c1, est_c2)
print(comparison)

# Example 2: Compare stratified seroincidence estimates
# Estimate seroincidence by catchment
est_by_catchment <- est_seroincidence_by(
  strata = c("catchment"),
  pop_data = sees_pop_data_pk_100,
  sr_params = typhoid_curves_nostrat_100,
  noise_params = example_noise_params_pk,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

# Compare all pairs of catchments - returns a table
comparisons_table <- compare_seroincidence(est_by_catchment)
print(comparisons_table)

# Example 3: Compare stratified estimates by multiple variables
est_by_multiple <- est_seroincidence_by(
  strata = c("catchment", "ageCat"),
  pop_data = sees_pop_data_pk_100,
  sr_params = typhoid_curves_nostrat_100,
  noise_params = example_noise_params_pk,
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

# Compare all pairs
comparisons_multi <- compare_seroincidence(est_by_multiple)
print(comparisons_multi)
