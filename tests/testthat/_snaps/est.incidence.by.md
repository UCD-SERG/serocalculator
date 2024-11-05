# est.incidence.by() produces expected results for typhoid data

    Code
      typhoid_results
    Output
      `seroincidence.by` object estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : catchment 
      
      This object is a list of `seroincidence` objects, with added meta-data attributes:`antigen_isos`   - Character vector of antigen isotypes used in analysis.
      `Strata`       - Input parameter strata of function `est.incidence.by()`
      
      Call the `summary()` function to obtain output results.

---

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

