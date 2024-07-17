# est.incidence() produces expected results for typhoid data

    Code
      typhoid_results
    Output
      `seroincidence` object estimated given the following setup:
      a) `antigen_isos`:  HlyE_IgG, HlyE_IgA 
      b) `lambda_start`:  0.1 
      Call the `summary()` function to obtain output results.
      Call the `autoplot()` function to graph the log-likelihood curve.

---

    structure(list(minimum = 523.575044823023, estimate = -1.7955958453869, 
        gradient = 3.60891331241403e-06, hessian = structure(86.991906300701, dim = c(1L, 
        1L)), code = 1L, iterations = 5L), class = c("seroincidence", 
    "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", "HlyE_IgA"
    ))

---

    Code
      typhoid_results_summary
    Output
      # A tibble: 1 x 10
        est.start incidence.rate     SE CI.lwr CI.upr coverage log.lik iterations
            <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
      1       0.1          0.166 0.0178  0.135  0.205     0.95   -524.          5
      # i 2 more variables: antigen.isos <chr>, nlm.convergence.code <ord>

---

    structure(list(est.start = 0.1, incidence.rate = 0.166028495555116, 
        SE = 0.0178009511621278, CI.lwr = 0.134561246947606, CI.upr = 0.204854383870479, 
        coverage = 0.95, log.lik = -523.575044823023, iterations = 5L, 
        antigen.isos = "HlyE_IgG+HlyE_IgA", nlm.convergence.code = structure(1L, levels = c("1", 
        "2", "3", "4", "5"), class = c("ordered", "factor"))), class = c("summary.seroincidence", 
    "tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L))

