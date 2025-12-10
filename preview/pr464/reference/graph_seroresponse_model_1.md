# graph antibody decay curves by antigen isotype

graph antibody decay curves by antigen isotype

## Usage

``` r
graph_seroresponse_model_1(
  object,
  antigen_isos = unique(object$antigen_iso),
  ncol = min(3, length(antigen_isos)),
  ...
)
```

## Arguments

- object:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) of curve
  parameters (one or more MCMC samples)

- antigen_isos:

  antigen isotypes to analyze (can subset `curve_params`)

- ncol:

  how many columns of subfigures to use in panel plot

- ...:

  Arguments passed on to
  [`plot_curve_params_one_ab`](https:/ucd-serg.github.io/serocalculator/preview/pr464/reference/plot_curve_params_one_ab.md)

  `verbose`

  :   verbose output

  `xlim`

  :   range of x values to graph

  `n_curves`

  :   how many curves to plot (see details).

  `n_points`

  :   Number of points to interpolate along the x axis (passed to
      [`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html))

  `iters_to_graph`

  :   which MCMC iterations in `curve_params` to plot (overrides
      `n_curves`).

  `alpha`

  :   (passed to
      [`ggplot2::geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html))
      how transparent the curves should be:

      - 0 = fully transparent (invisible)

      - 1 = fully opaque

  `log_x`

  :   should the x-axis be on a logarithmic scale (`TRUE`) or linear
      scale (`FALSE`, default)?

  `log_y`

  :   should the Y-axis be on a logarithmic scale (default, `TRUE`) or
      linear scale (`FALSE`)?

## Value

a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

## Details

### `iters_to_graph`

If you directly specify `iters_to_graph` when calling this function, the
row numbers are enumerated separately for each antigen isotype; in other
words, for the purposes of this argument, row numbers start over at 1
for each antigen isotype. There is currently no way to specify different
row numbers for different antigen isotypes; if you want to do that, you
will could call
[`plot_curve_params_one_ab()`](https:/ucd-serg.github.io/serocalculator/preview/pr464/reference/plot_curve_params_one_ab.md)
directly for each antigen isotype and combine the resulting panels
yourself. Or you could subset `curve_params` manually, before passing it
to this function, and set the `n_curves` argument to `Inf`.

## Examples

``` r
# \donttest{
library(dplyr)
library(ggplot2)
library(magrittr)

curve <-
  serocalculator_example("example_curve_params.csv") |>
  read.csv() |>
  as_sr_params() |>
  filter(antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")) |>
  graph_seroresponse_model_1()

curve

# }
```
