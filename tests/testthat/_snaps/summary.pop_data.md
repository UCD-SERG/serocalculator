# `summary.pop_data()` produces stable results when `strata = NULL`

    Code
      xs_data %>% summary(strata = NULL)
    Output
      
      n = 200 
      
      Distribution of age: 
      
      # A tibble: 1 x 7
            n   min first_quartile median  mean third_quartile   max
        <int> <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
      1   200   2.3           6.75     12  11.7             16    24
      
      Distributions of antigen-isotype measurements:
      
      # A tibble: 2 x 7
        antigen_iso   Min `1st Qu.` Median `3rd Qu.`   Max `# NAs`
        <fct>       <dbl>     <dbl>  <dbl>     <dbl> <dbl>   <int>
      1 HlyE_IgA    0          1.34   2.64      4.15  69.9       0
      2 HlyE_IgG    0.217      1.36   2.72      6.95  64.5       0
      

# `summary.pop_data()` produces stable results with stratification

    Code
      xs_data %>% summary(strata = "catchment")
    Output
      
      n = 200 
      
      Distribution of age: 
      
      # A tibble: 2 x 8
        catchment     n   min first_quartile median  mean third_quartile   max
        <chr>     <int> <dbl>          <dbl>  <dbl> <dbl>          <dbl> <dbl>
      1 kgh          94   2.3           6.85     11  11.6           15.9    24
      2 aku         106   2.3           6.6      12  11.8           16      23
      
      Distributions of antigen-isotype measurements:
      
      # A tibble: 4 x 8
        antigen_iso catchment    Min `1st Qu.` Median `3rd Qu.`   Max `# NAs`
        <fct>       <chr>      <dbl>     <dbl>  <dbl>     <dbl> <dbl>   <int>
      1 HlyE_IgA    kgh       0          1.55    2.90      3.99  45.6       0
      2 HlyE_IgA    aku       0.0308     1.20    2.00      5.74  69.9       0
      3 HlyE_IgG    kgh       0.362      1.99    2.79      8.06  64.5       0
      4 HlyE_IgG    aku       0.217      0.983   2.12      5.78  33.6       0
      

