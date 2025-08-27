# `est_seroincidence_by()` produces consistent results for sample data

    structure(list("Stratum 1" = structure(list(minimum = 269.456336163178, 
        estimate = -1.9659114149187, gradient = -9.97551535545146e-06, 
        hessian = structure(42.1287040808331, dim = c(1L, 1L)), code = 1L, 
        iterations = 4L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA")), "Stratum 2" = structure(list(minimum = 252.757931730411, 
        estimate = -1.61127190941552, gradient = 3.06923829056525e-06, 
        hessian = structure(44.1077020241307, dim = c(1L, 1L)), code = 1L, 
        iterations = 5L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA"))), antigen_isos = c("HlyE_IgG", "HlyE_IgA"), Strata = structure(list(
        Stratum = c("Stratum 1", "Stratum 2"), catchment = c("aku", 
        "kgh"), n = c(53L, 47L)), row.names = c(NA, -2L), class = c("tbl_df", 
    "tbl", "data.frame"), strata_vars = "catchment"), graphs_included = FALSE, class = c("seroincidence.by", 
    "list"))

# a warning is produced when `strata = NULL

    Code
      est_seroincidence_by(strata = NULL, pop_data = sees_pop_data_pk_100, sr_param = typhoid_curves_nostrat_100,
        noise_param = example_noise_params_pk, antigen_isos = c("HlyE_IgG",
          "HlyE_IgA"))
    Condition
      Warning:
      The `strata` argument to `est.incidence.by()` is missing.
      i If you do not want to stratify your data, consider using the `est_seroincidence()` function to simplify your code and avoid this warning.
      i Since the `strata` argument is empty, `est.incidence.by()` will return a <seroincidence> object, instead of a <seroincidence.by> object.
    Output
      `seroincidence` object estimated given the following setup:
      a) `antigen_isos`:  HlyE_IgG, HlyE_IgA 
      b) `lambda_start`:  0.1 
      Call the `summary()` function to obtain output results.
      Call the `autoplot()` function to graph the log-likelihood curve.

