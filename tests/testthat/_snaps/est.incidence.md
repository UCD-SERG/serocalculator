# est.incidence() produces expected results for typhoid data

    Code
      typhoid_results
    Output
      # A tibble: 1 x 11
        est.start incidence.rate      SE CI.lwr CI.upr coverage log.lik iterations
      *     <dbl>          <dbl>   <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
      1       0.1          0.128 0.00682  0.115  0.142     0.95  -2376.          4
      # i 3 more variables: antigen.isos <chr>, nlm.convergence.code <ord>,
      #   antigen.iso <chr>

