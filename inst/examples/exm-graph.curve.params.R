# Load example dataset
curve <- typhoid_curves_nostrat_100 |>
  dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

# Plot quantiles without showing all curves
plot1 <- graph.curve.params(curve, n_curves = 0)
print(plot1)

# Plot with additional quantiles and show all curves
plot2 <- graph.curve.params(
  curve,
  n_curves = Inf,
  quantiles = c(0.1, 0.5, 0.9)
)
print(plot2)
