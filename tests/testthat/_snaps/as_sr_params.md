# `as_sr_params()` produces expected results

    Code
      test_data
    Output
      # A tibble: 100 x 7
         antigen_iso  iter     y0     y1    t1     alpha     r
         <chr>       <int>  <dbl>  <dbl> <dbl>     <dbl> <dbl>
       1 HlyE_IgA        1  2.48    63.5  9.52 0.000581   1.75
       2 HlyE_IgG        1  3.04   164.   6.55 0.00457    1.17
       3 LPS_IgA         1  0.748  103.   4.98 0.00308    1.58
       4 LPS_IgG         1  0.941  320.   6.14 0.00166    1.41
       5 Vi_IgG          1  8.46  4348.   3.07 0.0000340  1.06
       6 HlyE_IgA        2  3.86   288.   1.27 0.000459   2.66
       7 HlyE_IgG        2  1.82   154.  10.8  0.000921   1.30
       8 LPS_IgA         2  1.76   852.   2.49 0.000126   2.91
       9 LPS_IgG         2  0.434   20.6  4.29 0.00122    1.37
      10 Vi_IgG          2 18.8    345.   3.48 0.000142   1.02
      # i 90 more rows

---

    WAoAAAACAAQEAQACAwAAAAMTAAAABwAAABAAAABkAAQACQAAAAhIbHlFX0lnQQAEAAkAAAAI
    SGx5RV9JZ0cABAAJAAAAB0xQU19JZ0EABAAJAAAAB0xQU19JZ0cABAAJAAAABlZpX0lnRwAE
    AAkAAAAISGx5RV9JZ0EABAAJAAAACEhseUVfSWdHAAQACQAAAAdMUFNfSWdBAAQACQAAAAdM
    UFNfSWdHAAQACQAAAAZWaV9JZ0cABAAJAAAACEhseUVfSWdBAAQACQAAAAhIbHlFX0lnRwAE
    AAkAAAAHTFBTX0lnQQAEAAkAAAAHTFBTX0lnRwAEAAkAAAAGVmlfSWdHAAQACQAAAAhIbHlF
    X0lnQQAEAAkAAAAISGx5RV9JZ0cABAAJAAAAB0xQU19JZ0EABAAJAAAAB0xQU19JZ0cABAAJ
    AAAABlZpX0lnRwAEAAkAAAAISGx5RV9JZ0EABAAJAAAACEhseUVfSWdHAAQACQAAAAdMUFNf
    SWdBAAQACQAAAAdMUFNfSWdHAAQACQAAAAZWaV9JZ0cABAAJAAAACEhseUVfSWdBAAQACQAA
    AAhIbHlFX0lnRwAEAAkAAAAHTFBTX0lnQQAEAAkAAAAHTFBTX0lnRwAEAAkAAAAGVmlfSWdH
    AAQACQAAAAhIbHlFX0lnQQAEAAkAAAAISGx5RV9JZ0cABAAJAAAAB0xQU19JZ0EABAAJAAAA
    B0xQU19JZ0cABAAJAAAABlZpX0lnRwAEAAkAAAAISGx5RV9JZ0EABAAJAAAACEhseUVfSWdH
    AAQACQAAAAdMUFNfSWdBAAQACQAAAAdMUFNfSWdHAAQACQAAAAZWaV9JZ0cABAAJAAAACEhs
    eUVfSWdBAAQACQAAAAhIbHlFX0lnRwAEAAkAAAAHTFBTX0lnQQAEAAkAAAAHTFBTX0lnRwAE
    AAkAAAAGVmlfSWdHAAQACQAAAAhIbHlFX0lnQQAEAAkAAAAISGx5RV9JZ0cABAAJAAAAB0xQ
    U19JZ0EABAAJAAAAB0xQU19JZ0cABAAJAAAABlZpX0lnRwAEAAkAAAAISGx5RV9JZ0EABAAJ
    AAAACEhseUVfSWdHAAQACQAAAAdMUFNfSWdBAAQACQAAAAdMUFNfSWdHAAQACQAAAAZWaV9J
    Z0cABAAJAAAACEhseUVfSWdBAAQACQAAAAhIbHlFX0lnRwAEAAkAAAAHTFBTX0lnQQAEAAkA
    AAAHTFBTX0lnRwAEAAkAAAAGVmlfSWdHAAQACQAAAAhIbHlFX0lnQQAEAAkAAAAISGx5RV9J
    Z0cABAAJAAAAB0xQU19JZ0EABAAJAAAAB0xQU19JZ0cABAAJAAAABlZpX0lnRwAEAAkAAAAI
    SGx5RV9JZ0EABAAJAAAACEhseUVfSWdHAAQACQAAAAdMUFNfSWdBAAQACQAAAAdMUFNfSWdH
    AAQACQAAAAZWaV9JZ0cABAAJAAAACEhseUVfSWdBAAQACQAAAAhIbHlFX0lnRwAEAAkAAAAH
    TFBTX0lnQQAEAAkAAAAHTFBTX0lnRwAEAAkAAAAGVmlfSWdHAAQACQAAAAhIbHlFX0lnQQAE
    AAkAAAAISGx5RV9JZ0cABAAJAAAAB0xQU19JZ0EABAAJAAAAB0xQU19JZ0cABAAJAAAABlZp
    X0lnRwAEAAkAAAAISGx5RV9JZ0EABAAJAAAACEhseUVfSWdHAAQACQAAAAdMUFNfSWdBAAQA
    CQAAAAdMUFNfSWdHAAQACQAAAAZWaV9JZ0cABAAJAAAACEhseUVfSWdBAAQACQAAAAhIbHlF
    X0lnRwAEAAkAAAAHTFBTX0lnQQAEAAkAAAAHTFBTX0lnRwAEAAkAAAAGVmlfSWdHAAQACQAA
    AAhIbHlFX0lnQQAEAAkAAAAISGx5RV9JZ0cABAAJAAAAB0xQU19JZ0EABAAJAAAAB0xQU19J
    Z0cABAAJAAAABlZpX0lnRwAEAAkAAAAISGx5RV9JZ0EABAAJAAAACEhseUVfSWdHAAQACQAA
    AAdMUFNfSWdBAAQACQAAAAdMUFNfSWdHAAQACQAAAAZWaV9JZ0cAAAANAAAAZAAAAAEAAAAB
    AAAAAQAAAAEAAAABAAAAAgAAAAIAAAACAAAAAgAAAAIAAAADAAAAAwAAAAMAAAADAAAAAwAA
    AAQAAAAEAAAABAAAAAQAAAAEAAAABQAAAAUAAAAFAAAABQAAAAUAAAAGAAAABgAAAAYAAAAG
    AAAABgAAAAcAAAAHAAAABwAAAAcAAAAHAAAACAAAAAgAAAAIAAAACAAAAAgAAAAJAAAACQAA
    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

