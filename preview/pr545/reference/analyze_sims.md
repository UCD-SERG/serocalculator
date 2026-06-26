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
    [`summary.seroincidence.by()`](https://ucd-serg.github.io/serocalculator/reference/summary.seroincidence.by.md)
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
#> 1       0.05          50 -0.00134     0.0123       0.0181  0.0177         0.0504
#> 2       0.05         200  0.00439     0.00669      0.00863 0.00949        0.0265
#> 3       0.8           50  0.220       0.148        0.237   0.319          0.590 
#> 4       0.8          200  0.156       0.0676       0.0948  0.181          0.266 
#> # ℹ 1 more variable: CI_Coverage <dbl>

# }
```
