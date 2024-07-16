# `est.incidence.by()` produces consistent results

    Code
      testobj %>% summary()
    Output
      Seroincidence estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : catchment 
      
       Seroincidence estimates:
      # A tibble: 2 x 13
        Stratum catchment     n est.start incidence.rate     SE CI.lwr CI.upr coverage
        <chr>   <chr>     <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
      1 Stratu~ aku         100       0.1          0.114 0.0135 0.0905  0.144     0.95
      2 Stratu~ kgh         100       0.1          0.217 0.0223 0.177   0.265     0.95
      # i 4 more variables: log.lik <dbl>, iterations <int>, antigen.isos <chr>,
      #   nlm.convergence.code <ord>

---

    structure(list("Stratum 1" = structure(list(minimum = 481.610713309434, 
        estimate = -2.17029142731673, gradient = -1.20481389489245e-06, 
        hessian = structure(71.0129484101985, dim = c(1L, 1L)), code = 1L, 
        iterations = 3L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA")), "Stratum 2" = structure(list(minimum = 525.683739400749, 
        estimate = -1.5283518623854, gradient = 1.45051240498471e-05, 
        hessian = structure(94.8163119574228, dim = c(1L, 1L)), code = 1L, 
        iterations = 5L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA"))), antigen_isos = c("HlyE_IgG", "HlyE_IgA"), Strata = structure(list(
        Stratum = c("Stratum 1", "Stratum 2"), catchment = c("aku", 
        "kgh"), n = c(100L, 100L)), row.names = c(NA, -2L), class = c("tbl_df", 
    "tbl", "data.frame"), strata_vars = "catchment"), graphs_included = FALSE, class = c("seroincidence.by", 
    "list"))

