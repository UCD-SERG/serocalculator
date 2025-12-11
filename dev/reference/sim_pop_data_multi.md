# Simulate multiple data sets

Simulate multiple data sets

## Usage

``` r
sim_pop_data_multi(
  nclus = 10,
  sample_sizes = 100,
  lambdas = c(0.05, 0.1, 0.15, 0.2, 0.3),
  num_cores = max(1, parallel::detectCores() - 1),
  rng_seed = 1234,
  verbose = FALSE,
  ...
)
```

## Arguments

- nclus:

  number of clusters

- sample_sizes:

  sample sizes to simulate

- lambdas:

  incidence rate, in events/person\*year

- num_cores:

  number of cores to use for parallel computations

- rng_seed:

  starting seed for random number generator, passed to
  [`rngtools::RNGseq()`](https://rdrr.io/pkg/rngtools/man/RNGseq.html)

- verbose:

  whether to report verbose information

- ...:

  Arguments passed on to
  [`sim_pop_data`](https://ucd-serg.github.io/serocalculator/dev/reference/sim_pop_data.md)

  `lambda`

  :   a [`numeric()`](https://rdrr.io/r/base/numeric.html) scalar
      indicating the incidence rate (in events per person-years)

  `n_samples`

  :   number of samples to simulate

  `age_range`

  :   age range of sampled individuals, in years

  `age_fixed`

  :   specify the curve parameters to use by age (does nothing at
      present?)

  `antigen_isos`

  :   Character vector with one or more antibody names. Values must
      match `curve_params`.

  `n_mcmc_samples`

  :   how many MCMC samples to use:

      - when `n_mcmc_samples` is in `1:4000` a fixed posterior sample is
        used

      - when `n_mcmc_samples` = `0`, a random sample is chosen

  `renew_params`

  :   whether to generate a new parameter set for each infection

      - `renew_params = TRUE` generates a new parameter set for each
        infection

      - `renew_params = FALSE` keeps the one selected at birth, but
        updates baseline y0

  `add_noise`

  :   a [`logical()`](https://rdrr.io/r/base/logical.html) indicating
      whether to add biological and measurement noise

  `noise_limits`

  :   biologic noise distribution parameters

  `format`

  :   a [`character()`](https://rdrr.io/r/base/character.html) variable,
      containing either:

      - `"long"` (one measurement per row) or

      - `"wide"` (one serum sample per row)

  `curve_params`

  :   a [`data.frame()`](https://rdrr.io/r/base/data.frame.html)
      containing MCMC samples of parameters from the Bayesian posterior
      distribution of a longitudinal decay curve model. The parameter
      columns must be named:

      - `antigen_iso`: a
        [`character()`](https://rdrr.io/r/base/character.html) vector
        indicating antigen-isotype combinations

      - `iter`: an [`integer()`](https://rdrr.io/r/base/integer.html)
        vector indicating MCMC sampling iterations

      - `y0`: baseline antibody level at \$t=0\$ (\$y(t=0)\$)

      - `y1`: antibody peak level (ELISA units)

      - `t1`: duration of infection

      - `alpha`: antibody decay rate (1/days for the current
        longitudinal parameter sets)

      - `r`: shape factor of antibody decay

## Value

a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
# \donttest{
# Load curve parameters
dmcmc <- typhoid_curves_nostrat_100

# Specify the antibody-isotype responses to include in analyses
antibodies <- c("HlyE_IgA", "HlyE_IgG")

# Set seed to reproduce results
set.seed(54321)

# Simulated incidence rate per person-year
lambdas = c(.05, .1, .15, .2, .3)

# Range covered in simulations
lifespan <- c(0, 10);

# Cross-sectional sample size
nrep <- 100

# Biologic noise distribution
dlims <- rbind(
  "HlyE_IgA" = c(min = 0, max = 0.5),
  "HlyE_IgG" = c(min = 0, max = 0.5)
)

sim_data <- sim_pop_data_multi(
  curve_params = dmcmc,
  lambdas = lambdas,
  sample_sizes = nrep,
  age_range = lifespan,
  antigen_isos = antibodies,
  n_mcmc_samples = 0,
  renew_params = TRUE,
  add_noise = TRUE,
  noise_limits = dlims,
  format = "long",
  nclus = 10)

sim_data
#> # A tibble: 10,000 × 7
#>      age id    antigen_iso value lambda.sim sample_size cluster
#>    <dbl> <chr> <chr>       <dbl>      <dbl>       <dbl>   <int>
#>  1  3.53 1     HlyE_IgA    0.757       0.05         100       1
#>  2  3.53 1     HlyE_IgG    0.520       0.05         100       1
#>  3  2.27 2     HlyE_IgA    0.819       0.05         100       1
#>  4  2.27 2     HlyE_IgG    0.707       0.05         100       1
#>  5  9.05 3     HlyE_IgA    0.150       0.05         100       1
#>  6  9.05 3     HlyE_IgG    0.506       0.05         100       1
#>  7  5.94 4     HlyE_IgA    0.837       0.05         100       1
#>  8  5.94 4     HlyE_IgG    0.870       0.05         100       1
#>  9  9.88 5     HlyE_IgA    0.297       0.05         100       1
#> 10  9.88 5     HlyE_IgG    0.272       0.05         100       1
#> # ℹ 9,990 more rows

# }
```
