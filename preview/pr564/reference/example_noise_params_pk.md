# Small example of noise parameters for typhoid

A subset of noise parameter estimates from the SEES study, for examples
and testing, for Pakistan

## Usage

``` r
example_noise_params_pk
```

## Format

### `example_noise_params_pk`

A `curve_params` object (from
[`as_sr_params()`](https://ucd-serg.github.io/serocalculator/reference/as_sr_params.md))
with 4 rows and 7 columns:

- antigen_iso:

  which antigen and isotype are being measured (data is in long format)

- Country:

  Location for which the noise parameters were estimated

- y.low:

  Lower limit of detection

- eps:

  Measurement noise: the bound on the relative measurement error. The
  relative error is modeled as uniform on the interval from `-eps` to
  `eps`, so `eps` is the largest relative deviation a measurement can
  have from its true value. Assay precision is often reported as a
  coefficient of variation (CV), the ratio of the standard deviation to
  the mean for replicates, ideally measured across plates rather than
  within the same plate; under this uniform model the CV equals
  `eps / sqrt(3)`, so a measured CV corresponds to `eps = sqrt(3) * CV`.

- nu:

  Biological noise: error from cross-reactivity to other antibodies. It
  is defined as the 95th percentile of the distribution of antibody
  responses to the antigen-isotype in a population with no exposure.

- y.high:

  Upper limit of detection

- Lab:

  Lab for which noise was estimated.

## Source

<https://osf.io/rtw5k>
