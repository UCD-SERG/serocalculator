# Analyze simulation results

Analyze simulation results

## Usage

``` r
analyze_sims(data)
```

## Arguments

- data:

  a
  [tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  with columns:

  - `lambda.sim`,

  - `incidence.rate`,

  - `SE`,

  - `CI.lwr`,

  - `CI.upr` for example, as produced by
    [`summary.seroincidence.by()`](https://ucd-serg.github.io/serocalculator/dev/reference/summary.seroincidence.by.md)
    with `lambda.sim` as a stratifying variable

## Value

a `sim_results` object (extends
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html))

## Examples

``` r
# \donttest{
dmcmc <- typhoid_curves_nostrat_100

n_cores <- 2

nclus <- 20
# cross-sectional sample size
nrep <- c(50, 200)

# incidence rate in e
lambdas <- c(.05, .8)

antibodies <- c("HlyE_IgA", "HlyE_IgG")
lifespan <- c(0, 10)
dlims = rbind(
"HlyE_IgA" = c(min = 0, max = 0.5),
"HlyE_IgG" = c(min = 0, max = 0.5)
)
sim_df <- sim_pop_data_multi(
n_cores = n_cores,
lambdas = lambdas,
nclus = nclus,
sample_sizes = nrep,
age_range = lifespan,
antigen_isos = antibodies,
renew_params = FALSE,
add_noise = TRUE,
curve_params = dmcmc,
noise_limits = dlims,
format = "long"
)
cond <- tibble::tibble(
antigen_iso = c("HlyE_IgG", "HlyE_IgA"),
nu = c(0.5, 0.5), # Biologic noise (nu)
eps = c(0, 0), # M noise (eps)
y.low = c(1, 1), # low cutoff (llod)
y.high = c(5e6, 5e6)
)
ests <-
est_seroincidence_by(
pop_data = sim_df,
sr_params = dmcmc,
noise_params = cond,
num_cores = n_cores,
strata = c("lambda.sim", "sample_size", "cluster"),
curve_strata_varnames = NULL,
noise_strata_varnames = NULL,
verbose = FALSE,
build_graph = FALSE, # slows down the function substantially
antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

ests |>
summary() |>
analyze_sims()
#> # A tibble: 4 × 8
#>   lambda.sim sample_size     Bias Mean_Est_SE Empirical_SE    RMSE Mean_CI_Width
#>        <dbl>       <dbl>    <dbl>       <dbl>        <dbl>   <dbl>         <dbl>
#> 1       0.05          50  3.20e-3     0.0130       0.0135  0.0135         0.0530
#> 2       0.05         200 -5.60e-4     0.00626      0.00793 0.00775        0.0248
#> 3       0.8           50  2.14e-1     0.144        0.189   0.282          0.571 
#> 4       0.8          200  1.95e-1     0.0710       0.0879  0.213          0.279 
#> # ℹ 1 more variable: CI_Coverage <dbl>

# }
```
