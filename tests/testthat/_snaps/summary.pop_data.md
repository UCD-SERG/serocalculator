# `summary.pop_data()` expected same results

    Code
      xs_data %>% summary(strata = "Country")
    Output
      
      n = 3336 
      
      Distribution of age: 
      
      # A tibble: 3 x 7
        Country      min first_quartile median  mean third_quartile   max
        <chr>      <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
      1 Bangladesh   0.6            4.9    9.2  9.43           14    18  
      2 Nepal        0.9            5.3   10.8 11.3            16.7  25  
      3 Pakistan     0.8            4.9    9   10.1            14.8  24.3
      
      Distributions of antigen-isotype measurements:
      
        antigen_iso    Country Min   1st Qu.   Median  3rd Qu.      Max # NAs
      1    HlyE_IgA Bangladesh   0 0.9810911 2.123017 5.000199 218.6229     0
      2    HlyE_IgG Bangladesh   0 0.9810911 2.123017 5.000199 218.6229     0
      3    HlyE_IgA      Nepal   0 0.9810911 2.123017 5.000199 218.6229     0
      4    HlyE_IgG      Nepal   0 0.9810911 2.123017 5.000199 218.6229     0
      5    HlyE_IgA   Pakistan   0 0.9810911 2.123017 5.000199 218.6229     0
      6    HlyE_IgG   Pakistan   0 0.9810911 2.123017 5.000199 218.6229     0

