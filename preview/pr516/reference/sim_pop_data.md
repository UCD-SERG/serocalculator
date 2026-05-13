# Simulate a cross-sectional serosurvey with noise

Makes a cross-sectional data set (age, y(t) set) and adds noise, if
desired.

## Usage

``` r
sim_pop_data(
  lambda = 0.1,
  n_samples = 100,
  age_range = c(0, 20),
  age_fixed = NA,
  antigen_isos = intersect(get_biomarker_levels(curve_params), rownames(noise_limits)),
  n_mcmc_samples = 0,
  renew_params = FALSE,
  add_noise = FALSE,
  curve_params,
  noise_limits,
  format = "wide",
  verbose = FALSE,
  ...
)
```

## Arguments

- lambda:

  a [`numeric()`](https://rdrr.io/r/base/numeric.html) scalar indicating
  the incidence rate (in events per person-years)

- n_samples:

  number of samples to simulate

- age_range:

  age range of sampled individuals, in years

- age_fixed:

  specify the curve parameters to use by age (does nothing at present?)

- antigen_isos:

  Character vector with one or more antibody names. Values must match
  `curve_params`.

- n_mcmc_samples:

  how many MCMC samples to use:

  - when `n_mcmc_samples` is in `1:4000` a fixed posterior sample is
    used

  - when `n_mcmc_samples` = `0`, a random sample is chosen

- renew_params:

  whether to generate a new parameter set for each infection

  - `renew_params = TRUE` generates a new parameter set for each
    infection

  - `renew_params = FALSE` keeps the one selected at birth, but updates
    baseline y0

- add_noise:

  a [`logical()`](https://rdrr.io/r/base/logical.html) indicating
  whether to add biological and measurement noise

- curve_params:

  a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) containing
  MCMC samples of parameters from the Bayesian posterior distribution of
  a longitudinal decay curve model. The parameter columns must be named:

  - `antigen_iso`: a
    [`character()`](https://rdrr.io/r/base/character.html) vector
    indicating antigen-isotype combinations

  - `iter`: an [`integer()`](https://rdrr.io/r/base/integer.html) vector
    indicating MCMC sampling iterations

  - `y0`: baseline antibody level at \$t=0\$ (\$y(t=0)\$)

  - `y1`: antibody peak level (ELISA units)

  - `t1`: duration of infection

  - `alpha`: antibody decay rate (1/days for the current longitudinal
    parameter sets)

  - `r`: shape factor of antibody decay

- noise_limits:

  biologic noise distribution parameters

- format:

  a [`character()`](https://rdrr.io/r/base/character.html) variable,
  containing either:

  - `"long"` (one measurement per row) or

  - `"wide"` (one serum sample per row)

- verbose:

  logical: if TRUE, print verbose log information to console

- ...:

  Arguments passed on to
  [`simcs.tinf`](https://ucd-serg.github.io/serocalculator/reference/simcs.tinf.md),
  [`ldpar`](https://ucd-serg.github.io/serocalculator/reference/ldpar.md),
  [`ab`](https://ucd-serg.github.io/serocalculator/reference/ab.md),
  [`mk_baseline`](https://ucd-serg.github.io/serocalculator/reference/mk_baseline.md)

  `age`

  :   age at infection

  `nmc`

  :   mcmc sample to use

  `npar`

  :   number of parameters

  `t`

  :   [numeric](https://rdrr.io/r/base/numeric.html)
      [vector](https://rdrr.io/r/base/vector.html) of elapsed times
      since start of infection

  `par`

  :   [numeric](https://rdrr.io/r/base/numeric.html)
      [matrix](https://rdrr.io/r/base/matrix.html) of model parameters:

      - rows are parameters

      - columns are biomarkers

  `kab`

  :   [integer](https://rdrr.io/r/base/integer.html) indicating which
      row to read from `blims`

  `n`

  :   number of observations

  `blims`

  :   range of possible baseline antibody levels

## Value

a
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing simulated cross-sectional serosurvey data, with columns:

- `age`: age (in days)

- one column for each element in the `antigen_iso` input argument

## Examples

``` r
# Load curve parameters
dmcmc <- typhoid_curves_nostrat_100

# Specify the antibody-isotype responses to include in analyses
antibodies <- c("HlyE_IgA", "HlyE_IgG")

# Set seed to reproduce results
set.seed(54321)

# Simulated incidence rate per person-year
lambda <- 0.2
# Range covered in simulations
lifespan <- c(0, 10)
# Cross-sectional sample size
nrep <- 100

# Biologic noise distribution
dlims <- rbind(
  "HlyE_IgA" = c(min = 0, max = 0.5),
  "HlyE_IgG" = c(min = 0, max = 0.5)
)

# Generate cross-sectional data
csdata <- sim_pop_data(
  curve_params = dmcmc,
  lambda = lambda,
  n_samples = nrep,
  age_range = lifespan,
  antigen_isos = antibodies,
  n_mcmc_samples = 0,
  renew_params = TRUE,
  add_noise = TRUE,
  noise_limits = dlims,
  format = "long"
)
```
