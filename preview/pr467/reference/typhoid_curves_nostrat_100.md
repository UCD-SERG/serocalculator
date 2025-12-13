# Small example of antibody response curve parameters for typhoid

A subset of data from the SEES study, for examples and testing.

## Usage

``` r
typhoid_curves_nostrat_100
```

## Format

### `typhoid_curves_nostrat_100`

A `curve_params` object (from
[`as_sr_params()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/as_sr_params.md))
with 500 rows and 7 columns:

- antigen_iso:

  which antigen and isotype are being measured (data is in long format)

- iter:

  MCMC iteration

- y0:

  Antibody concentration at t = 0 (start of active infection)

- y1:

  Antibody concentration at t = `t1` (end of active infection)

- t1:

  Duration of active infection

- alpha:

  Antibody decay rate coefficient

- r:

  Antibody decay rate exponent parameter

## Source

<https://osf.io/rtw5k>
