# Small example cross-sectional data set

A subset of data from the SEES data, for examples and testing, data from
Pakistan only, variable names not normalized by
[`as_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr475/reference/as_pop_data.md).

## Usage

``` r
sees_pop_data_pk_100_old_names
```

## Format

### `sees_pop_data_pk_100_old_names`

A `pop_data` object (from
[`as_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr475/reference/as_pop_data.md))
with 200 rows and 8 columns:

- index_id:

  Observation ID

- Country:

  Country where the participant was living

- cluster:

  survey sampling cluster

- catchment:

  survey catchment area

- Age:

  participant's age when sampled, in years

- antigen_iso:

  which antigen and isotype are being measured (data is in long format)

- result:

  concentration of antigen isotype, in ELISA units

## Source

<https://osf.io/n6cp3>
