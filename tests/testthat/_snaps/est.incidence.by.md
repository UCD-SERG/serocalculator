# est.incidence.by() produces expected results for typhoid data

    Code
      typhoid_results
    Output
      `seroincidence.by` object estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : Country 
      
      This object is a list of `seroincidence` objects, with added meta-data attributes:`antigen_isos`   - Character vector of antigen isotypes used in analysis.
      `Strata`       - Input parameter strata of function `est.incidence.by()`
      
      Call the `summary()` function to obtain output results.

---

    structure(list("Stratum 1" = structure(list(minimum = 260.546868067556, 
        estimate = -2.02114708582914, gradient = 3.37492026736821e-07, 
        hessian = structure(37.570481481294, dim = c(1L, 1L)), code = 1L, 
        iterations = 4L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA"))), antigen_isos = c("HlyE_IgG", "HlyE_IgA"), Strata = structure(list(
        Stratum = "Stratum 1", Country = "Pakistan", n = 50L), row.names = c(NA, 
    -1L), class = c("tbl_df", "tbl", "data.frame"), strata_vars = "Country"), graphs_included = FALSE, class = c("seroincidence.by", 
    "list"))

