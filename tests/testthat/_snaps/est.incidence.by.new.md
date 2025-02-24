# a warning is produced when `strata = NULL

    Code
      est.incidence.by(strata = NULL, pop_data = sees_pop_data_pk_100, sr_param = typhoid_curves_nostrat_100,
        noise_param = example_noise_params_pk, antigen_isos = c("HlyE_IgG",
          "HlyE_IgA"))
    Condition
      Warning:
      The `strata` argument to `est.incidence.by()` is missing.
      i If you do not want to stratify your data, consider using the `est.incidence()` function to simplify your code and avoid this warning.
      i Since the `strata` argument is empty, `est.incidence.by()` will return a <seroincidence> object, instead of a <seroincidence.by> object.
    Output
      `seroincidence` object estimated given the following setup:
      a) `antigen_isos`:  HlyE_IgG, HlyE_IgA 
      b) `lambda_start`:  0.1 
      Call the `summary()` function to obtain output results.
      Call the `autoplot()` function to graph the log-likelihood curve.

