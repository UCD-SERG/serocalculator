# Graph an antibody decay curve model

Graph an antibody decay curve model

## Usage

``` r
plot_curve_params_one_ab(
  object,
  verbose = FALSE,
  alpha = 0.4,
  n_curves = 100,
  n_points = 1000,
  log_x = FALSE,
  log_y = TRUE,
  iters_to_graph = seq_len(min(n_curves, nrow(object))),
  xlim = c(10^-1, 10^3.1),
  ...
)
```

## Arguments

- object:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) of curve
  parameters (one or more MCMC samples)

- verbose:

  verbose output

- alpha:

  (passed to
  [`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html))
  how transparent the curves should be:

  - 0 = fully transparent (invisible)

  - 1 = fully opaque

- n_curves:

  how many curves to plot (see details).

- n_points:

  Number of points to interpolate along the x axis (passed to
  [`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html))

- log_x:

  should the x-axis be on a logarithmic scale (`TRUE`) or linear scale
  (`FALSE`, default)?

- log_y:

  should the Y-axis be on a logarithmic scale (default, `TRUE`) or
  linear scale (`FALSE`)?

- iters_to_graph:

  which MCMC iterations in `curve_params` to plot (overrides
  `n_curves`).

- xlim:

  range of x values to graph

- ...:

  Additional arguments passed to
  [`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html).
  Arguments `fun`, `n`, and `args` are fixed by this helper and should
  not be supplied.

## Value

a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

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
# \donttest{
library(dplyr) # loads the `%>%` operator and `dplyr::filter()`

curve <-
  typhoid_curves_nostrat_100 %>%
  filter(antigen_iso == ("HlyE_IgG")) %>%
  serocalculator:::plot_curve_params_one_ab()

  curve

# }
```
