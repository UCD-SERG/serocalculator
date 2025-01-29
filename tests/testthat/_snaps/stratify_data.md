# stratify_data() produces consistent results

    Code
      stratified_data
    Output
      $`Stratum 1`
      $pop_data
      # A tibble: 106 x 3
          value   age antigen_iso
          <dbl> <dbl> <fct>      
       1 5.69    18   HlyE_IgA   
       2 1.23     7.3 HlyE_IgA   
       3 1.08     2.6 HlyE_IgA   
       4 1.43     3.9 HlyE_IgA   
       5 3.06    13   HlyE_IgA   
       6 3.10    14.4 HlyE_IgA   
       7 0.0308   3.4 HlyE_IgA   
       8 6.39    22   HlyE_IgA   
       9 0.240    4   HlyE_IgA   
      10 3.45    14.8 HlyE_IgA   
      # i 96 more rows
      
      $curve_params
      # A tibble: 200 x 4
            y1     alpha     r antigen_iso
         <dbl>     <dbl> <dbl> <fct>      
       1  63.5 0.000581   1.75 HlyE_IgA   
       2 288.  0.000459   2.66 HlyE_IgA   
       3 432.  0.000277   1.61 HlyE_IgA   
       4  30.6 0.00127    1.87 HlyE_IgA   
       5 160.  0.00140    1.40 HlyE_IgA   
       6 525.  0.000294   2.26 HlyE_IgA   
       7  30.8 0.00459    1.55 HlyE_IgA   
       8  41.3 0.00234    1.48 HlyE_IgA   
       9 248.  0.0000467  8.50 HlyE_IgA   
      10 319.  0.000448   1.75 HlyE_IgA   
      # i 190 more rows
      
      $noise_params
      # A tibble: 2 x 5
           nu   eps y.low  y.high antigen_iso
        <dbl> <dbl> <dbl>   <dbl> <chr>      
      1  2.60 0.279 0.508 5000000 HlyE_IgA   
      2  2.36 0.146 1.59  5000000 HlyE_IgG   
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      $`Stratum 2`
      $pop_data
      # A tibble: 94 x 3
         value   age antigen_iso
         <dbl> <dbl> <fct>      
       1 0.568  13.2 HlyE_IgA   
       2 0.779  11   HlyE_IgA   
       3 1.90   12   HlyE_IgA   
       4 1.41   16   HlyE_IgA   
       5 7.12    7.6 HlyE_IgA   
       6 0       2.3 HlyE_IgA   
       7 2.90    4   HlyE_IgA   
       8 1.44    5.1 HlyE_IgA   
       9 3.77   18.7 HlyE_IgA   
      10 2.80    7   HlyE_IgA   
      # i 84 more rows
      
      $curve_params
      # A tibble: 200 x 4
            y1     alpha     r antigen_iso
         <dbl>     <dbl> <dbl> <fct>      
       1  63.5 0.000581   1.75 HlyE_IgA   
       2 288.  0.000459   2.66 HlyE_IgA   
       3 432.  0.000277   1.61 HlyE_IgA   
       4  30.6 0.00127    1.87 HlyE_IgA   
       5 160.  0.00140    1.40 HlyE_IgA   
       6 525.  0.000294   2.26 HlyE_IgA   
       7  30.8 0.00459    1.55 HlyE_IgA   
       8  41.3 0.00234    1.48 HlyE_IgA   
       9 248.  0.0000467  8.50 HlyE_IgA   
      10 319.  0.000448   1.75 HlyE_IgA   
      # i 190 more rows
      
      $noise_params
      # A tibble: 2 x 5
           nu   eps y.low  y.high antigen_iso
        <dbl> <dbl> <dbl>   <dbl> <chr>      
      1  2.60 0.279 0.508 5000000 HlyE_IgA   
      2  2.36 0.146 1.59  5000000 HlyE_IgG   
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      attr(,"antigen_isos")
      [1] HlyE_IgA HlyE_IgG
      Levels: HlyE_IgA HlyE_IgG
      attr(,"strata")
      # A tibble: 2 x 3
        Stratum   catchment     n
        <chr>     <chr>     <int>
      1 Stratum 1 aku          53
      2 Stratum 2 kgh          47
      attr(,"class")
      [1] "biomarker_data_and_params.list" "list"                          

