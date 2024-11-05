# est.incidence() produces expected results for typhoid data

    Code
      summary(typhoid_results)
    Output
      # A tibble: 1 x 10
        est.start incidence.rate     SE CI.lwr CI.upr coverage log.lik iterations
            <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
      1       0.1          0.166 0.0178  0.135  0.205     0.95   -524.          5
      # i 2 more variables: antigen.isos <chr>, nlm.convergence.code <ord>

---

    structure(list(minimum = 523.575044823023, estimate = -1.7955958453869, 
        gradient = 3.60891331241403e-06, hessian = structure(86.991906300701, dim = c(1L, 
        1L)), code = 1L, iterations = 5L), class = c("seroincidence", 
    "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", "HlyE_IgA"
    ))

