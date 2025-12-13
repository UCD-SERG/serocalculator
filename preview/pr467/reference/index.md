# Package index

## Prepare data, kinetics models, and noise parameters for analysis

- [`as_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/as_pop_data.md)
  : Load a cross-sectional antibody survey data set

- [`load_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/load_pop_data.md)
  : Load a cross-sectional antibody survey data set

- [`as_sr_params()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/as_sr_params.md)
  : Load longitudinal seroresponse parameters

- [`load_sr_params()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/load_sr_params.md)
  : Load longitudinal seroresponse parameter samples

- [`as_noise_params()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/as_noise_params.md)
  : Load noise parameters

- [`load_noise_params()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/load_noise_params.md)
  : Load noise parameters

- [`check_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/check_pop_data.md)
  : Check the formatting of a cross-sectional antibody survey dataset.

- [`strata()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/strata.md)
  :

  Extract `Strata` metadata from an object

## Extract information from data, seroresponse models, and noise parameters

- [`get_biomarker_levels()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/get_biomarker_levels.md)
  : Extract biomarker levels
- [`get_biomarker_names_var()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/get_biomarker_names_var.md)
  : Get biomarker variable name
- [`get_values_var()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/get_values_var.md)
  : Extract antibody measurement values
- [`get_values()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/get_values.md)
  : Get antibody measurement values

## Summarize data

- [`summary(`*`<pop_data>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/summary.pop_data.md)
  [`print(`*`<summary.pop_data>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/summary.pop_data.md)
  : Summarize cross-sectional antibody survey data

## Visualize data

- [`autoplot(`*`<pop_data>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.pop_data.md)
  : Plot distribution of antibodies
- [`autoplot(`*`<curve_params>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.curve_params.md)
  : Graph antibody decay curves by antigen isotype
- [`graph.curve.params()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/graph.curve.params.md)
  : Graph estimated antibody decay curves
- [`graph_loglik()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/graph_loglik.md)
  : Graph log-likelihood of data
- [`log_likelihood()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/log_likelihood.md)
  : Calculate log-likelihood

## Estimate seroconversion incidence rates

- [`est_seroincidence()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/est_seroincidence.md)
  : Find the maximum likelihood estimate of the incidence rate parameter
- [`est_seroincidence_by()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/est_seroincidence_by.md)
  : Estimate Seroincidence

## Summarize seroconversion incidence rate estimates

- [`summary(`*`<seroincidence>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/summary.seroincidence.md)
  : Summarizing fitted seroincidence models

- [`summary(`*`<seroincidence.by>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/summary.seroincidence.by.md)
  :

  Summary Method for `"seroincidence.by"` Objects

## Visualize seroconversion incidence rate estimates

- [`autoplot(`*`<seroincidence>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.seroincidence.md)
  : Plot the log-likelihood curve for the incidence rate estimate

- [`autoplot(`*`<seroincidence.by>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.seroincidence.by.md)
  :

  Plot `seroincidence.by` log-likelihoods

- [`autoplot(`*`<summary.seroincidence.by>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.summary.seroincidence.by.md)
  :

  Plot method for `summary.seroincidence.by` objects

## Example data sets

- [`sees_pop_data_100`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/sees_pop_data_100.md)
  : Small example cross-sectional data set

- [`sees_pop_data_pk_100`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/sees_pop_data_pk_100.md)
  : Small example cross-sectional data set

- [`typhoid_curves_nostrat_100`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/typhoid_curves_nostrat_100.md)
  : Small example of antibody response curve parameters for typhoid

- [`example_noise_params_sees`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/example_noise_params_sees.md)
  : Small example of noise parameters for typhoid

- [`example_noise_params_pk`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/example_noise_params_pk.md)
  : Small example of noise parameters for typhoid

- [`sees_typhoid_ests_strat`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/sees_typhoid_ests_strat.md)
  :

  Example `"seroincidence.by"` object

- [`serocalculator_example()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/serocalculator_example.md)
  : Get path to an example file

## Simulate data sets

- [`sim_pop_data()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/sim_pop_data.md)
  : Simulate a cross-sectional serosurvey with noise

- [`sim_pop_data_multi()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/sim_pop_data_multi.md)
  : Simulate multiple data sets

- [`analyze_sims()`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/analyze_sims.md)
  : Analyze simulation results

- [`autoplot(`*`<sim_results>`*`)`](https:/ucd-serg.github.io/serocalculator/preview/pr467/reference/autoplot.sim_results.md)
  :

  Plot simulation results
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method for `sim_results` objects
