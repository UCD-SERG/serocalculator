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

  Arguments passed on to
  [`ggplot2::geom_function`](https://ggplot2.tidyverse.org/reference/geom_function.html)

  `mapping`

  :   Set of aesthetic mappings created by
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
      specified and `inherit.aes = TRUE` (the default), it is combined
      with the default mapping at the top level of the plot. You must
      supply `mapping` if there is no plot mapping.

  `data`

  :   Ignored by
      [`stat_function()`](https://ggplot2.tidyverse.org/reference/geom_function.html),
      do not use.

  `stat`

  :   The statistical transformation to use on the data for this layer.
      When using a `geom_*()` function to construct a layer, the `stat`
      argument can be used to override the default coupling between
      geoms and stats. The `stat` argument accepts the following:

      - A `Stat` ggproto subclass, for example `StatCount`.

      - A string naming the stat. To give the stat as a string, strip
        the function name of the `stat_` prefix. For example, to use
        [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
        give the stat as `"count"`.

      - For more information and other ways to specify the stat, see the
        [layer
        stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
        documentation.

  `position`

  :   A position adjustment to use on the data for this layer. This can
      be used in various ways, including to prevent overplotting and
      improving the display. The `position` argument accepts the
      following:

      - The result of calling a position function, such as
        [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
        This method allows for passing extra arguments to the position.

      - A string naming the position adjustment. To give the position as
        a string, strip the function name of the `position_` prefix. For
        example, to use
        [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
        give the position as `"jitter"`.

      - For more information and other ways to specify the position, see
        the [layer
        position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
        documentation.

  `arrow`

  :   Arrow specification, as created by
      [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

  `arrow.fill`

  :   fill colour to use for the arrow head (if closed). `NULL` means
      use `colour` aesthetic.

  `lineend`

  :   Line end style (round, butt, square).

  `linejoin`

  :   Line join style (round, mitre, bevel).

  `linemitre`

  :   Line mitre limit (number greater than 1).

  `na.rm`

  :   If `FALSE`, the default, missing values are removed with a
      warning. If `TRUE`, missing values are silently removed.

  `show.legend`

  :   logical. Should this layer be included in the legends? `NA`, the
      default, includes if any aesthetics are mapped. `FALSE` never
      includes, and `TRUE` always includes. It can also be a named
      logical vector to finely select the aesthetics to display. To
      include legend keys for all levels, even when no data exists, use
      `TRUE`. If `NA`, all levels are shown in legend, but unobserved
      levels are omitted.

  `inherit.aes`

  :   If `FALSE`, overrides the default aesthetics, rather than
      combining with them. This is most useful for helper functions that
      define both data and aesthetics and shouldn't inherit behaviour
      from the default plot specification, e.g.
      [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

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
