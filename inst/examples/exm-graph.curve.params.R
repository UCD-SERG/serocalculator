# Load example dataset
curve <- typhoid_curves_nostrat_100 |>
  dplyr::filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG"))

# Plot without showing all curves
plot1 <- graph.curve.params(curve)
print(plot1)

# Plot with additional quantiles and show all curves
plot2 <- graph.curve.params(
  curve,
  show_all_curves = TRUE,
  quantiles = c(0.1, 0.5, 0.9)
)
print(plot2)
