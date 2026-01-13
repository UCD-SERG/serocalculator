# Enteric Fever Seroincidence Vignette

## Introduction

This vignette provides users with an example analysis using the
[**serocalculator**](https://github.com/UCD-SERG/serocalculator) package
by reproducing the analysis for: [**Estimating typhoid incidence from
community-based serosurveys: a multicohort
study**](https://www.thelancet.com/journals/lanmic/article/PIIS2666-5247(22)00114-8/fulltext)
(Aiemjoy et al. (2022)). We review the methods underlying the analysis
and then walk through an example of enteric fever incidence in Pakistan.
Note that because this is a simplified version of the analysis, the
results here will differ slightly from those presented in the
publication.

In this example, users will determine the seroincidence of enteric fever
in cross-sectional serosurveys conducted as part of the SeroEpidemiology
and Environmental Surveillance (SEES) for enteric fever study in
Bangladesh, Nepal, and Pakistan. Longitudinal antibody responses were
modeled from 1420 blood culture-confirmed enteric fever cases enrolled
from the same countries.

## Methods

### Load packages

The first step in conducting this analysis is to load our necessary
packages. **If you haven’t installed already, you will need to do so
before loading:**

``` r
install.packages("serocalculator")
```

See the [Installation
instructions](https://ucd-serg.github.io/serocalculator/#installing-the-serocalculator-package)
for more details.

Once the `serocalculator` package has been installed, load it into your
R environment using [`library()`](https://rdrr.io/r/base/library.html),
along with any other packages you may need for data management; For this
example, we will load `tidyverse` and `forcats`:

``` r
library(serocalculator)
library(tidyverse)
library(forcats)
```

## Load data

Pathogen-specific sample datasets, noise parameters, and longitudinal
antibody dynamics for **serocalculator** are available on the
[Serocalculator Data Repository](https://osf.io/ne8pc/) on Open Science
Framework (OSF). We will pull this data directly into our R environment.

Note that each dataset has specific formatting and variable name
requirements.

### Load and prepare longitudinal parameter data

We will first load the longitudinal curve parameters to set the antibody
decay parameters. In this example, these parameters were modeled with
Bayesian hierarchical models to fit two-phase power-function decay
models to the longitudinal antibody responses among confirmed enteric
fever cases.

*Formatting Specifications*: Data should be imported as a “wide”
dataframe with one column for each parameter and one row for each
iteration of the posterior distribution for each antigen isotype. Column
names must *exactly* match follow the naming conventions:

| Column Name | Description                         |
|-------------|-------------------------------------|
| y0          | Baseline antibody concentration     |
| y1          | Peak antibody concentration         |
| t1          | Time to peak antibody concentration |
| alpha       | Antibody decay rate                 |
| r           | Antibody decay shape                |

*Note that variable names are case-sensitive*

``` r
# Import longitudinal antibody parameters from OSF
curves <-
  "https://osf.io/download/rtw5k/" |>
  load_sr_params()
```

### Visualize curve parameters

We can graph the decay curves with an
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method:

``` r
# Visualize curve parameters
curves |>
  filter(
    antigen_iso %in% c("HlyE_IgA", "HlyE_IgG")
  ) |>
  autoplot()
```

![](enteric_fever_example_files/figure-html/unnamed-chunk-3-1.png)

### Load and prepare cross-sectional data

Next, we load our sample cross-sectional data. We will use a subset of
results from the SEES dataset. Ideally, this will be a representative
sample of the general population without regard to disease status.
Later, we will limit our analysis to cross-sectional data from Pakistan.

We have selected hemolysin E (*HlyE*) as our target antigen and *IgG*
and *IgA* as our target immunoglobulin isotypes. Users may select
different serologic markers depending on what is available in your data.
From the original dataset, we rename our result and age variables to the
names required by **serocalculator**.

*Formatting Specifications*: Cross-sectional population data should be a
“long” dataframe with one column for each variable and one row for each
antigen isotype resulted for an individual. So the same individual will
have more than one row if they have results for more than one antigen
isotype. The dataframe can have additional variables, but the two below
are required:

| Column Name | Description                    |
|-------------|--------------------------------|
| value       | Quantitative antibody response |
| age         | Numeric age                    |

*Note that variable names are case sensitive*

``` r
#Import cross-sectional data from OSF and rename required variables
xs_data <- readr::read_rds("https://osf.io/download//n6cp3/") |>
  as_pop_data()
```

### Summarize antibody data

We can compute numerical summaries of our cross-sectional antibody data
with a [`summary()`](https://rdrr.io/r/base/summary.html) method for
`pop_data` objects:

``` r
xs_data |> summary()
#> 
#> n = 3336 
#> 
#> Distribution of age: 
#> 
#> # A tibble: 1 × 7
#>       n   min first_quartile median  mean third_quartile   max
#>   <int> <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
#> 1  3336   0.6              5     10  10.5             15    25
#> 
#> Distributions of antigen-isotype measurements:
#> 
#> # A tibble: 2 × 7
#>   antigen_iso   Min `1st Qu.` Median `3rd Qu.`   Max `# NAs`
#>   <fct>       <dbl>     <dbl>  <dbl>     <dbl> <dbl>   <int>
#> 1 HlyE_IgA        0     0.851   1.74      3.66  133.       0
#> 2 HlyE_IgG        0     1.15    2.70      6.74  219.       0
```

### Visualize antibody data

We examine our cross-sectional antibody data by visualizing the
distribution of quantitative antibody responses. Here, we will look at
the distribution of our selected antigen and isotype pairs, HlyE IgA and
HlyE IgG, across participating countries.

``` r

#color palette
country_pal <- c("#EA6552", "#8F4B86", "#0099B4FF")

xs_data |> autoplot(strata = "Country", type = "density") +
  scale_fill_manual(values = country_pal)
```

![](enteric_fever_example_files/figure-html/plots-1.png)

We see that across countries, our data is highly skewed with the
majority of responses on the lower end of our data with long tails.
Let’s get a better look at the distribution by log transforming our
antibody response value.

``` r
# Create log transformed plots

xs_data |>
  mutate(Country = fct_relevel(Country, "Bangladesh", "Pakistan", "Nepal")) |>
  autoplot(strata = "Country", type = "density") +
  scale_fill_manual(values = country_pal) +
  scale_x_log10(labels = scales::label_comma())
#> Warning in scale_x_log10(labels = scales::label_comma()): log-10
#> transformation introduced infinite values.
#> Warning: Removed 18 rows containing non-finite outside the scale range
#> (`stat_density()`).
```

![](enteric_fever_example_files/figure-html/logplot-1.png)

Once log transformed, our data looks much more normally distributed. In
most cases, log transformation will be the best way to visualize
serologic data.

Let’s also take a look at how antibody responses change by age.

``` r

#Plot antibody responses by age
xs_data |>
  autoplot(
    strata = "Country",
    type = "age-scatter"
  ) +
  scale_color_manual(values = country_pal)
```

![](enteric_fever_example_files/figure-html/plot-age-1.png)

In this plot, we can see that the highest burden is in Bangladesh, but
Nepal has the steepest slope and experiences the greatest change in
seroconversion rates by age, with lower exposures at younger ages and
higher exposures at older ages.

### Load noise parameters

Next, we must set conditions based on some assumptions about the data
and errors that may need to be accounted for. This will differ based on
background knowledge of the data.

The biological noise, $\nu$ (“nu”), represents error from
cross-reactivity to other antibodies. It is defined as the 95th
percentile of the distribution of antibody responses to the
antigen-isotype in a population with no exposure.

Measurement noise, $\varepsilon$ (“epsilon”), represents measurement
error from the laboratory testing process. It is defined by a CV
(coefficient of variation) as the ratio of the standard deviation to the
mean for replicates. Note that the CV should ideally be measured across
plates rather than within the same plate.

*Formatting Specifications*: Noise parameter data should be a dataframe
with one row for each antigen isotype and columns for each noise
parameter below.

| Column Name | Description                           |
|-------------|---------------------------------------|
| y.low       | Lower limit of detection of the assay |
| nu          | Biologic noise                        |
| y.high      | Upper limit of detection of the assay |
| eps         | Measurement noise                     |

*Note that variable names are case-sensitive.*

``` r
# Import noise parameters from OSF

noise <- url("https://osf.io/download//hqy4v/") |> readRDS()
```

## Estimate Seroincidence

Now we are ready to begin estimating seroincidence. We will conduct two
separate analyses using two distinct functions, `est.incidence` and
`est.incidence.by`, to calculate the overall seroincidence and the
stratified seroincidence, respectively.

### Overall Seroincidence

Using the function `est.incidence`, we filter to sites in Pakistan and
define the datasets for our cross-sectional data (pop_data),
longitudinal parameters (curve_param), and noise parameters
(noise_param). We also define the antigen-isotype pairs to be included
in the estimate (antigen_isos). Here, we have chosen to use two antigen
isotypes, but users can add additional pairs if available.

``` r
# Using est.incidence (no strata)

est1 <- est_seroincidence(
  pop_data = xs_data |> filter(Country == "Pakistan"),
  sr_params = curves,
  noise_params = noise |> filter(Country == "Pakistan"),
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

summary(est1)
#> # A tibble: 1 × 10
#>   est.start incidence.rate      SE CI.lwr CI.upr coverage log.lik iterations
#>       <dbl>          <dbl>   <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
#> 1       0.1          0.128 0.00682  0.115  0.142     0.95  -2376.          4
#> # ℹ 2 more variables: antigen.isos <chr>, nlm.convergence.code <ord>
```

### Stratified Seroincidence

We can also produce stratified seroincidence estimates. Users can select
one or more stratification variables in their cross-sectional population
dataset. Let’s compare estimates across all countries and by age group.

``` r
#Using est.incidence.by (strata)

est_country_age <- est_seroincidence_by(
  strata = c("Country", "ageCat"),
  pop_data = xs_data,
  sr_params = curves,
  curve_strata_varnames = NULL,
  noise_params = noise,
  noise_strata_varnames = "Country",
  antigen_isos = c("HlyE_IgG", "HlyE_IgA"),
  num_cores = 8 # Allow for parallel processing to decrease run time
)
#> Warning: The number of observations in `data` varies between antigen isotypes, for at
#> least one stratum.
#> Sample size for each stratum will be calculated as the minimum number of
#> observations across all antigen isotypes.
#> # A tibble: 6 × 4
#>   Country ageCat antigen_iso     n
#>   <chr>   <fct>  <fct>       <int>
#> 1 Nepal   <5     HlyE_IgA      171
#> 2 Nepal   <5     HlyE_IgG      179
#> 3 Nepal   5-15   HlyE_IgA      378
#> 4 Nepal   5-15   HlyE_IgG      390
#> 5 Nepal   16+    HlyE_IgA      211
#> 6 Nepal   16+    HlyE_IgG      217

summary(est_country_age)
#> Seroincidence estimated given the following setup:
#> a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
#> b) Strata       : Country, ageCat 
#> 
#>  Seroincidence estimates:
#> # A tibble: 9 × 14
#>   Stratum   Country  ageCat     n est.start incidence.rate      SE CI.lwr CI.upr
#>   <chr>     <chr>    <fct>  <int>     <dbl>          <dbl>   <dbl>  <dbl>  <dbl>
#> 1 Stratum 1 Banglad… <5       101       0.1         0.400  0.0395  0.330  0.485 
#> 2 Stratum 2 Banglad… 5-15     256       0.1         0.477  0.0320  0.418  0.544 
#> 3 Stratum 3 Banglad… 16+       44       0.1         0.449  0.0763  0.322  0.627 
#> 4 Stratum 4 Nepal    <5       171       0.1         0.0203 0.00444 0.0132 0.0311
#> 5 Stratum 5 Nepal    5-15     378       0.1         0.0355 0.00311 0.0299 0.0421
#> 6 Stratum 6 Nepal    16+      211       0.1         0.0935 0.00776 0.0795 0.110 
#> 7 Stratum 7 Pakistan <5       126       0.1         0.106  0.0136  0.0823 0.136 
#> 8 Stratum 8 Pakistan 5-15     261       0.1         0.115  0.00845 0.0991 0.132 
#> 9 Stratum 9 Pakistan 16+      107       0.1         0.190  0.0204  0.154  0.235 
#> # ℹ 5 more variables: coverage <dbl>, log.lik <dbl>, iterations <int>,
#> #   antigen.isos <chr>, nlm.convergence.code <ord>
```

Note that we get a warning about uneven observations between antigen
isotypes, meaning some participants do not have results for both *HlyE
IgA* and *HlyE IgG*. The warning indicates that the “*Sample size for
each stratum will be calculated as the minimum number of observations
across all antigen isotypes*”, so only participants with both antigen
isotypes are included. To avoid this, filter the dataset to include only
records with all specified antigen isotypes.

We set `curve_strata_varnames = NULL` to avoid stratification in the
“curves” dataset because it does not include our strata variables.
Without this, a warning appears: “*curve_params is missing all strata
variables, and will be used unstratified*”. To stratify based on
variables that exist in a longitudinal curve parameters dataset, specify
variables using `curve_strata_varnames`, similar to how
`noise_strata_varnames` is used for “noise” above.

Finally, let’s visualize our seroincidence estimates by country and age
category using
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html):

``` r
# Save summary(est_country_age)
est_country_agedf <- summary(est_country_age)

# Plot seroincidence estimates
autoplot(
  est_country_agedf,
  type = "bar",
  yvar = "ageCat",
  color_var = "Country",
  color_palette = country_pal,
  CIs = TRUE
)
```

![](enteric_fever_example_files/figure-html/unnamed-chunk-5-1.png)

## Comparing Seroincidence Rates

After estimating seroincidence rates across different groups, we may
want to statistically compare these rates to determine if observed
differences are significant. The
[`compare_seroincidence()`](https:/ucd-serg.github.io/serocalculator/preview/pr470/reference/compare_seroincidence.md)
function performs two-sample z-tests to compare seroincidence rates.

### Comparing Two Individual Estimates

First, let’s compare seroincidence rates between two countries directly.
We’ll estimate seroincidence for Bangladesh and Nepal separately, then
compare them:

``` r
# Estimate seroincidence for Bangladesh
est_bangladesh <- est_seroincidence(
  pop_data = xs_data |> filter(Country == "Bangladesh"),
  sr_params = curves,
  noise_params = noise |> filter(Country == "Bangladesh"),
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

# Estimate seroincidence for Nepal
est_nepal <- est_seroincidence(
  pop_data = xs_data |> filter(Country == "Nepal"),
  sr_params = curves,
  noise_params = noise |> filter(Country == "Nepal"),
  antigen_isos = c("HlyE_IgG", "HlyE_IgA")
)

# Compare the two estimates
comparison <- compare_seroincidence(est_bangladesh, est_nepal)
print(comparison)
#> 
#>  Two-sample z-test for difference in seroincidence rates
#> 
#> data:  seroincidence estimates
#> z = 17.027, p-value < 2.2e-16
#> alternative hypothesis: true difference in incidence rates is not equal to 0
#> 95 percent confidence interval:
#>  0.3559738 0.4485881
#> sample estimates:
#> incidence rate 1 incidence rate 2       difference 
#>       0.45050113       0.04822018       0.40228095
```

The output follows the standard `htest` format in R, providing the
z-statistic, p-value, estimates for both groups, and a confidence
interval for the difference in incidence rates.

### Comparing All Pairs of Stratified Estimates

For stratified analyses, we can compare all pairs of strata at once.
This is particularly useful when we have multiple groups and want a
comprehensive view of all pairwise differences:

``` r
# Compare all pairs of country-age combinations
comparisons_table <- compare_seroincidence(est_country_age)

# Display the results
print(comparisons_table)
#> # A tibble: 36 × 14
#>    Stratum_1 Stratum_2 Country.1  ageCat.1 Country.2  ageCat.2 incidence.rate.1
#>  * <chr>     <chr>     <chr>      <fct>    <chr>      <fct>               <dbl>
#>  1 Stratum 1 Stratum 2 Bangladesh <5       Bangladesh 5-15                0.400
#>  2 Stratum 1 Stratum 3 Bangladesh <5       Bangladesh 16+                 0.400
#>  3 Stratum 1 Stratum 4 Bangladesh <5       Nepal      <5                  0.400
#>  4 Stratum 1 Stratum 5 Bangladesh <5       Nepal      5-15                0.400
#>  5 Stratum 1 Stratum 6 Bangladesh <5       Nepal      16+                 0.400
#>  6 Stratum 1 Stratum 7 Bangladesh <5       Pakistan   <5                  0.400
#>  7 Stratum 1 Stratum 8 Bangladesh <5       Pakistan   5-15                0.400
#>  8 Stratum 1 Stratum 9 Bangladesh <5       Pakistan   16+                 0.400
#>  9 Stratum 2 Stratum 3 Bangladesh 5-15     Bangladesh 16+                 0.477
#> 10 Stratum 2 Stratum 4 Bangladesh 5-15     Nepal      <5                  0.477
#> # ℹ 26 more rows
#> # ℹ 7 more variables: incidence.rate.2 <dbl>, difference <dbl>, SE <dbl>,
#> #   z.statistic <dbl>, p.value <dbl>, CI.lwr <dbl>, CI.upr <dbl>
```

This produces a table with one row for each pairwise comparison,
showing: - The two strata being compared - Incidence rates for each
group - The difference in rates - Standard error of the difference -
Z-statistic - P-value - 95% confidence interval for the difference

We can use this to identify which differences are statistically
significant (typically using p \< 0.05 as a threshold).

## Conclusions

We estimate that Bangladesh has the highest enteric fever seroconversion
rates across all age groups, with the highest rates observed among 5- to
15-year-olds (477 per 1000 person-years). In this age group, the
seroconversion rate in Bangladesh is 14 times higher than in Nepal,
where the rate is 35 per 1000 person-years. These findings highlight
substantial geographic variation in enteric fever transmission,
emphasizing the need for targeted prevention strategies.
**serocalculator** offers an efficient and reproducible approach to
estimating seroconversion rates, enabling data-driven insights for
disease surveillance and public health decision-making.

## Acknowledgments

We gratefully acknowledge the study participants for their valuable time
and interest in participating in these studies. Special thanks to our
collaborators at Sabin Vaccine Institute, Aga Khan University (Karachi,
Pakistan), Child Health Research Foundation (Dhaka, Bangladesh), and
Dhulikhel Hospital, Kathmandu University Hospital (Dhulikhel, Nepal).

## Funding

This project was supported by grants from the National Institutes of
Health (NIH) National Institute of Allergy and Infectious Disease
(R21AI176416), the NIH Fogarty International Center (K01TW012177) and
the Bill and Melinda Gates Foundation.

## References

Aiemjoy, K., Seidman J. C., Saha S., Munira S. J., Islam Sajib M. S.,
and Sarkar Sium S. M. al. 2022. “Estimating Typhoid Incidence from
Community-Based Serosurveys: A Multicohort Study.” *The Lancet Microbe*
3 (8): e578–87. <https://doi.org/10.1016/S2666-5247(22)00114-8>.
