# `as_noise_params()` produces expected results

    Code
      test_data
    Output
      # A tibble: 16 x 7
         antigen_iso Country    y.low   eps    nu  y.high Lab  
         <chr>       <fct>      <dbl> <dbl> <dbl>   <dbl> <fct>
       1 HlyE_IgA    Bangladesh 0.376 0.280  2.60 5000000 CHRF 
       2 HlyE_IgA    Ghana      0.179 0.240  2.60 5000000 MGH  
       3 HlyE_IgA    Nepal      0.853 0.238  2.60 5000000 DH   
       4 HlyE_IgA    Pakistan   0.508 0.279  2.60 5000000 AKU  
       5 HlyE_IgG    Bangladesh 0.787 0.306  2.36 5000000 CHRF 
       6 HlyE_IgG    Ghana      0.645 0.164  2.36 5000000 MGH  
       7 HlyE_IgG    Nepal      1.89  0.128  2.36 5000000 DH   
       8 HlyE_IgG    Pakistan   1.59  0.146  2.36 5000000 AKU  
       9 LPS_IgA     Bangladesh 0.660 0.299  2.14 5000000 CHRF 
      10 LPS_IgA     Ghana      0.861 0.163  2.14 5000000 MGH  
      11 LPS_IgA     Nepal      1.79  0.115  2.14 5000000 DH   
      12 LPS_IgA     Pakistan   5.13  0.246  2.14 5000000 AKU  
      13 LPS_IgG     Bangladesh 0.992 0.298  3.24 5000000 CHRF 
      14 LPS_IgG     Ghana      0.885 0.195  3.24 5000000 MGH  
      15 LPS_IgG     Nepal      0.647 0.179  3.24 5000000 DH   
      16 LPS_IgG     Pakistan   4.84  0.273  3.24 5000000 AKU  

---

    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
    AAAHTFBTX0lnQQAEAAkAAAAHTFBTX0lnRwAAAP4=

