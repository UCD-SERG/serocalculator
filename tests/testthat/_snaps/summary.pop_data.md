# `summary.pop_data()` produces stable results when `strata = NULL`

    Code
      warnings <- character()
      result <- withCallingHandlers(xs_data %>% summary(strata = NULL), warning = function(
        w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
      list(result = result, warnings = warnings)
    Output
      $result
      
      n = 3336 
      
      Distribution of age: 
      
      # A tibble: 1 x 7
            n   min first_quartile median  mean third_quartile   max
        <int> <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
      1  3336   0.6              5     10  10.5             15    25
      
      Distributions of antigen-isotype measurements:
      
      # A tibble: 2 x 7
        antigen_iso   Min `1st Qu.` Median `3rd Qu.`   Max `# NAs`
        <fct>       <dbl>     <dbl>  <dbl>     <dbl> <dbl>   <int>
      1 HlyE_IgA        0     0.851   1.74      3.66  133.       0
      2 HlyE_IgG        0     1.15    2.70      6.74  219.       0
      
      
      $warnings
      character(0)
      

# `summary.pop_data()` produces stable results with stratification

    Code
      xs_data %>% summary(strata = "Country")
    Output
      
      n = 3336 
      
      Distribution of age: 
      
      # A tibble: 3 x 8
        Country        n   min first_quartile median  mean third_quartile   max
        <chr>      <int> <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
      1 Bangladesh   802   0.6            4.9    9.2  9.43           14    18  
      2 Nepal       1546   0.9            5.3   10.8 11.3            16.7  25  
      3 Pakistan     988   0.8            4.9    9   10.1            14.8  24.3
      
      Distributions of antigen-isotype measurements:
      
      # A tibble: 6 x 8
        antigen_iso Country       Min `1st Qu.` Median `3rd Qu.`   Max `# NAs`
        <fct>       <chr>       <dbl>     <dbl>  <dbl>     <dbl> <dbl>   <int>
      1 HlyE_IgA    Bangladesh 0.0418     2.11    3.58      6.70 113.        0
      2 HlyE_IgG    Bangladesh 0.119      4.97    9.32     18.9  219.        0
      3 HlyE_IgA    Nepal      0          0.563   1.02      2.05  57.5       0
      4 HlyE_IgG    Nepal      0          0.897   1.62      3.37 184.        0
      5 HlyE_IgA    Pakistan   0          1.13    2.12      3.89 133.        0
      6 HlyE_IgG    Pakistan   0.192      1.04    2.40      5.15 135.        0
      

