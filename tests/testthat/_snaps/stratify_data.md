# stratify_data() produces consistent results

    Code
      stratified_data
    Output
      $`Stratum 1`
      $pop_data
      # A tibble: 588 x 3
         value   age antigen_iso
         <dbl> <dbl> <fct>      
       1 5.69   18   HlyE_IgA   
       2 4.21   18   HlyE_IgG   
       3 1.23    7.3 HlyE_IgA   
       4 3.00    7.3 HlyE_IgG   
       5 1.08    2.6 HlyE_IgA   
       6 0.217   2.6 HlyE_IgG   
       7 1.43    3.9 HlyE_IgA   
       8 0.956   3.9 HlyE_IgG   
       9 3.06   13   HlyE_IgA   
      10 9.57   13   HlyE_IgG   
      # i 578 more rows
      
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
      # A tibble: 16 x 5
            nu   eps y.low  y.high antigen_iso
         <dbl> <dbl> <dbl>   <dbl> <chr>      
       1  2.60 0.280 0.376 5000000 HlyE_IgA   
       2  2.60 0.240 0.179 5000000 HlyE_IgA   
       3  2.60 0.238 0.853 5000000 HlyE_IgA   
       4  2.60 0.279 0.508 5000000 HlyE_IgA   
       5  2.36 0.306 0.787 5000000 HlyE_IgG   
       6  2.36 0.164 0.645 5000000 HlyE_IgG   
       7  2.36 0.128 1.89  5000000 HlyE_IgG   
       8  2.36 0.146 1.59  5000000 HlyE_IgG   
       9  2.14 0.299 0.660 5000000 LPS_IgA    
      10  2.14 0.163 0.861 5000000 LPS_IgA    
      11  2.14 0.115 1.79  5000000 LPS_IgA    
      12  2.14 0.246 5.13  5000000 LPS_IgA    
      13  3.24 0.298 0.992 5000000 LPS_IgG    
      14  3.24 0.195 0.885 5000000 LPS_IgG    
      15  3.24 0.179 0.647 5000000 LPS_IgG    
      16  3.24 0.273 4.84  5000000 LPS_IgG    
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      $`Stratum 2`
      $pop_data
      # A tibble: 802 x 3
          value   age antigen_iso
          <dbl> <dbl> <fct>      
       1  7.24    4   HlyE_IgA   
       2 24.0     4   HlyE_IgG   
       3  0.836   3.7 HlyE_IgA   
       4  3.17    3.7 HlyE_IgG   
       5  3.42   10.6 HlyE_IgA   
       6 14.3    10.6 HlyE_IgG   
       7  8.84   16   HlyE_IgA   
       8 14.2    16   HlyE_IgG   
       9 10.3    16.5 HlyE_IgA   
      10 29.4    16.5 HlyE_IgG   
      # i 792 more rows
      
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
      # A tibble: 16 x 5
            nu   eps y.low  y.high antigen_iso
         <dbl> <dbl> <dbl>   <dbl> <chr>      
       1  2.60 0.280 0.376 5000000 HlyE_IgA   
       2  2.60 0.240 0.179 5000000 HlyE_IgA   
       3  2.60 0.238 0.853 5000000 HlyE_IgA   
       4  2.60 0.279 0.508 5000000 HlyE_IgA   
       5  2.36 0.306 0.787 5000000 HlyE_IgG   
       6  2.36 0.164 0.645 5000000 HlyE_IgG   
       7  2.36 0.128 1.89  5000000 HlyE_IgG   
       8  2.36 0.146 1.59  5000000 HlyE_IgG   
       9  2.14 0.299 0.660 5000000 LPS_IgA    
      10  2.14 0.163 0.861 5000000 LPS_IgA    
      11  2.14 0.115 1.79  5000000 LPS_IgA    
      12  2.14 0.246 5.13  5000000 LPS_IgA    
      13  3.24 0.298 0.992 5000000 LPS_IgG    
      14  3.24 0.195 0.885 5000000 LPS_IgG    
      15  3.24 0.179 0.647 5000000 LPS_IgG    
      16  3.24 0.273 4.84  5000000 LPS_IgG    
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      $`Stratum 3`
      $pop_data
      # A tibble: 653 x 3
          value   age antigen_iso
          <dbl> <dbl> <fct>      
       1 46.5     8.7 HlyE_IgA   
       2  1.79    8.7 HlyE_IgG   
       3  0.504  20.5 HlyE_IgA   
       4  0.949  20.5 HlyE_IgG   
       5  5.07   10.5 HlyE_IgG   
       6  1.07   23.2 HlyE_IgA   
       7  0.461  23.2 HlyE_IgG   
       8  0.422   4.4 HlyE_IgA   
       9  0.334   4.4 HlyE_IgG   
      10  1.46   14   HlyE_IgA   
      # i 643 more rows
      
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
      # A tibble: 16 x 5
            nu   eps y.low  y.high antigen_iso
         <dbl> <dbl> <dbl>   <dbl> <chr>      
       1  2.60 0.280 0.376 5000000 HlyE_IgA   
       2  2.60 0.240 0.179 5000000 HlyE_IgA   
       3  2.60 0.238 0.853 5000000 HlyE_IgA   
       4  2.60 0.279 0.508 5000000 HlyE_IgA   
       5  2.36 0.306 0.787 5000000 HlyE_IgG   
       6  2.36 0.164 0.645 5000000 HlyE_IgG   
       7  2.36 0.128 1.89  5000000 HlyE_IgG   
       8  2.36 0.146 1.59  5000000 HlyE_IgG   
       9  2.14 0.299 0.660 5000000 LPS_IgA    
      10  2.14 0.163 0.861 5000000 LPS_IgA    
      11  2.14 0.115 1.79  5000000 LPS_IgA    
      12  2.14 0.246 5.13  5000000 LPS_IgA    
      13  3.24 0.298 0.992 5000000 LPS_IgG    
      14  3.24 0.195 0.885 5000000 LPS_IgG    
      15  3.24 0.179 0.647 5000000 LPS_IgG    
      16  3.24 0.273 4.84  5000000 LPS_IgG    
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      $`Stratum 4`
      $pop_data
      # A tibble: 893 x 3
           value   age antigen_iso
           <dbl> <dbl> <fct>      
       1  4.15    13.7 HlyE_IgA   
       2 22.8     13.7 HlyE_IgG   
       3  0.0675  11.2 HlyE_IgA   
       4  2.48    11.2 HlyE_IgG   
       5  1.25    16.7 HlyE_IgA   
       6  6.10    16.7 HlyE_IgG   
       7  0.627    9.4 HlyE_IgA   
       8  1.18     9.4 HlyE_IgG   
       9  2.12    14.2 HlyE_IgA   
      10  4.75    14.2 HlyE_IgG   
      # i 883 more rows
      
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
      # A tibble: 16 x 5
            nu   eps y.low  y.high antigen_iso
         <dbl> <dbl> <dbl>   <dbl> <chr>      
       1  2.60 0.280 0.376 5000000 HlyE_IgA   
       2  2.60 0.240 0.179 5000000 HlyE_IgA   
       3  2.60 0.238 0.853 5000000 HlyE_IgA   
       4  2.60 0.279 0.508 5000000 HlyE_IgA   
       5  2.36 0.306 0.787 5000000 HlyE_IgG   
       6  2.36 0.164 0.645 5000000 HlyE_IgG   
       7  2.36 0.128 1.89  5000000 HlyE_IgG   
       8  2.36 0.146 1.59  5000000 HlyE_IgG   
       9  2.14 0.299 0.660 5000000 LPS_IgA    
      10  2.14 0.163 0.861 5000000 LPS_IgA    
      11  2.14 0.115 1.79  5000000 LPS_IgA    
      12  2.14 0.246 5.13  5000000 LPS_IgA    
      13  3.24 0.298 0.992 5000000 LPS_IgG    
      14  3.24 0.195 0.885 5000000 LPS_IgG    
      15  3.24 0.179 0.647 5000000 LPS_IgG    
      16  3.24 0.273 4.84  5000000 LPS_IgG    
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      $`Stratum 5`
      $pop_data
      # A tibble: 400 x 3
          value   age antigen_iso
          <dbl> <dbl> <fct>      
       1  0.568  13.2 HlyE_IgA   
       2  2.15   13.2 HlyE_IgG   
       3  0.779  11   HlyE_IgA   
       4  1.89   11   HlyE_IgG   
       5  1.90   12   HlyE_IgA   
       6  8.09   12   HlyE_IgG   
       7  1.41   16   HlyE_IgA   
       8  2.37   16   HlyE_IgG   
       9  7.12    7.6 HlyE_IgA   
      10 11.6     7.6 HlyE_IgG   
      # i 390 more rows
      
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
      # A tibble: 16 x 5
            nu   eps y.low  y.high antigen_iso
         <dbl> <dbl> <dbl>   <dbl> <chr>      
       1  2.60 0.280 0.376 5000000 HlyE_IgA   
       2  2.60 0.240 0.179 5000000 HlyE_IgA   
       3  2.60 0.238 0.853 5000000 HlyE_IgA   
       4  2.60 0.279 0.508 5000000 HlyE_IgA   
       5  2.36 0.306 0.787 5000000 HlyE_IgG   
       6  2.36 0.164 0.645 5000000 HlyE_IgG   
       7  2.36 0.128 1.89  5000000 HlyE_IgG   
       8  2.36 0.146 1.59  5000000 HlyE_IgG   
       9  2.14 0.299 0.660 5000000 LPS_IgA    
      10  2.14 0.163 0.861 5000000 LPS_IgA    
      11  2.14 0.115 1.79  5000000 LPS_IgA    
      12  2.14 0.246 5.13  5000000 LPS_IgA    
      13  3.24 0.298 0.992 5000000 LPS_IgG    
      14  3.24 0.195 0.885 5000000 LPS_IgG    
      15  3.24 0.179 0.647 5000000 LPS_IgG    
      16  3.24 0.273 4.84  5000000 LPS_IgG    
      
      attr(,"class")
      [1] "biomarker_data_and_params" "list"                     
      
      attr(,"antigen_isos")
      [1] HlyE_IgA HlyE_IgG
      Levels: HlyE_IgA HlyE_IgG
      attr(,"strata")
      # A tibble: 5 x 3
        Stratum   catchment     n
        <chr>     <chr>     <int>
      1 Stratum 1 aku         294
      2 Stratum 2 dhaka       401
      3 Stratum 3 kathmandu   322
      4 Stratum 4 kavre       438
      5 Stratum 5 kgh         200
      attr(,"class")
      [1] "biomarker_data_and_params.list" "list"                          

