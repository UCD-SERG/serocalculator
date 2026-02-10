# Graph estimated antibody decay curves

Graph estimated antibody decay curves

## Usage

``` r
graph.curve.params(
  object,
  antigen_isos = unique(object$antigen_iso),
  verbose = FALSE,
  quantiles = c(0.1, 0.5, 0.9),
  alpha_samples = 0.3,
  chain_color = TRUE,
  log_x = FALSE,
  log_y = TRUE,
  n_curves = 100,
  iters_to_graph = head(unique(object$iter), n_curves),
  ...
)
```

## Arguments

- object:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) containing
  MCMC samples of antibody decay curve parameters

- antigen_isos:

  antigen isotypes to analyze (can subset `object`)

- verbose:

  verbose output

- quantiles:

  Optional [numeric](https://rdrr.io/r/base/numeric.html)
  [vector](https://rdrr.io/r/base/vector.html) of point-wise (over time)
  quantiles to plot (e.g., 10%, 50%, and 90% = `c(0.1, 0.5, 0.9)`). If
  `NULL`, no quantile lines are shown.

- alpha_samples:

  `alpha` parameter passed to
  [ggplot2::geom_line](https://ggplot2.tidyverse.org/reference/geom_path.html)
  (has no effect if `iters_to_graph` is empty)

- chain_color:

  [logical](https://rdrr.io/r/base/logical.html): if
  [TRUE](https://rdrr.io/r/base/logical.html) (default), MCMC chain
  lines are colored by chain. If
  [FALSE](https://rdrr.io/r/base/logical.html), all MCMC chain lines are
  black.

- log_x:

  should the x-axis be on a logarithmic scale (`TRUE`) or linear scale
  (`FALSE`, default)?

- log_y:

  should the Y-axis be on a logarithmic scale (default, `TRUE`) or
  linear scale (`FALSE`)?

- n_curves:

  how many curves to plot (see details).

- iters_to_graph:

  which MCMC iterations in `curve_params` to plot (overrides
  `n_curves`).

- ...:

  not currently used

## Value

a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object showing the antibody dynamic kinetics of selected antigen/isotype
combinations, with optional posterior distribution quantile curves.

## Details

### `n_curves` and `iters_to_graph`

In most cases, `object` will contain too many rows of MCMC samples for
all of these samples to be plotted at once.

- Setting the `n_curves` argument to a value smaller than the number of
  rows in `curve_params` will cause this function to select the first
  `n_curves` rows to graph.

- Setting `n_curves` larger than the number of rows in \` will result
  all curves being plotted.

- If the user directly specifies the `iters_to_graph` argument, then
  `n_curves` has no effect.

## Examples

``` r
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


# Plot with MCMC chains in black
plot3 <- graph.curve.params(
  curve,
  n_curves = Inf,
  quantiles = c(0.1, 0.5, 0.9),
  chain_color = FALSE
)
print(plot3)
```
