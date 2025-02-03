# print method works consistently

    Code
      summary(typhoid_results)
    Output
      Seroincidence estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : catchment 
      
       Seroincidence estimates:
      # A tibble: 2 x 13
        Stratum catchment     n est.start incidence.rate     SE CI.lwr CI.upr coverage
        <chr>   <chr>     <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
      1 Stratu~ aku          53       0.1          0.140 0.0216  0.104  0.189     0.95
      2 Stratu~ kgh          47       0.1          0.200 0.0301  0.149  0.268     0.95
      # i 4 more variables: log.lik <dbl>, iterations <int>, antigen.isos <chr>,
      #   nlm.convergence.code <ord>

