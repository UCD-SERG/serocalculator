# print method works consistently

    Code
      summary(typhoid_results)
    Output
      Seroincidence estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : catchment 
      
       Seroincidence estimates:
      # A tibble: 2 x 14
        Stratum  catchment     n est.start incidence.rate     SE CI.lwr CI.upr se_type
        <chr>    <chr>     <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl> <chr>  
      1 Stratum~ aku          53       0.1          0.140 0.0216  0.104  0.189 standa~
      2 Stratum~ kgh          47       0.1          0.200 0.0301  0.149  0.268 standa~
      # i 5 more variables: coverage <dbl>, log.lik <dbl>, iterations <int>,
      #   antigen.isos <chr>, nlm.convergence.code <ord>

