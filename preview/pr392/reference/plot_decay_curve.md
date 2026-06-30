# Plot a decay function

Plot a decay function

## Usage

``` r
plot_decay_curve(
  decay_function = antibody_decay_curve,
  ...,
  xmax = 100,
  ymax = NA,
  title = ""
)
```

## Arguments

- decay_function:

  a function with first argument `t`

- ...:

  parameters passed to `decay_function`

- xmax:

  upper limit of x axis

- ymax:

  upper limit of y axis

- title:

  plot title

## Value

a [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
plot_decay_curve(antibody_decay_curve)
```
