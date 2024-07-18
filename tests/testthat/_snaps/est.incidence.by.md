# `est.incidence.by()` produces consistent results

    Code
      strat_ests
    Output
      `seroincidence.by` object estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : catchment 
      
      This object is a list of `seroincidence` objects, with added meta-data attributes:
      `antigen_isos` - Character vector of antigen isotypes used in analysis.
      `Strata`       - Input parameter strata of function `est.incidence.by()`
      
      Call the `summary()` function to obtain output results.

---

    structure(list("Stratum 1" = structure(list(minimum = 481.610713309434, 
        estimate = -2.17029142731673, gradient = -1.20481389489245e-06, 
        hessian = structure(71.0129484101985, dim = c(1L, 1L)), code = 1L, 
        iterations = 3L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA")), "Stratum 2" = structure(list(minimum = 525.683739400749, 
        estimate = -1.5283518623854, gradient = 1.45051240498471e-05, 
        hessian = structure(94.8163119574228, dim = c(1L, 1L)), code = 1L, 
        iterations = 5L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA"))), antigen_isos = c("HlyE_IgG", "HlyE_IgA"), Strata = structure(list(
        Stratum = c("Stratum 1", "Stratum 2"), catchment = c("aku", 
        "kgh"), n = c(100L, 100L)), row.names = c(NA, -2L), class = c("tbl_df", 
    "tbl", "data.frame"), strata_vars = "catchment"), graphs_included = FALSE, class = c("seroincidence.by", 
    "list"))

---

    Code
      strat_ests_summary
    Output
      Seroincidence estimated given the following setup:
      a) Antigen isotypes   : HlyE_IgG, HlyE_IgA 
      b) Strata       : catchment 
      
       Seroincidence estimates:
      # A tibble: 2 x 13
        Stratum catchment     n est.start incidence.rate     SE CI.lwr CI.upr coverage
        <chr>   <chr>     <int>     <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
      1 Stratu~ aku         100       0.1          0.114 0.0135 0.0905  0.144     0.95
      2 Stratu~ kgh         100       0.1          0.217 0.0223 0.177   0.265     0.95
      # i 4 more variables: log.lik <dbl>, iterations <int>, antigen.isos <chr>,
      #   nlm.convergence.code <ord>

---

    WAoAAAACAAQEAAACAwAAAAMTAAAADQAAABAAAAACAAQACQAAAAlTdHJhdHVtIDEABAAJAAAA
    CVN0cmF0dW0gMgAAABAAAAACAAQACQAAAANha3UABAAJAAAAA2tnaAAAAA0AAAACAAAAZAAA
    AGQAAAAOAAAAAj+5mZmZmZmaP7mZmZmZmZoAAAAOAAAAAj+9OJBemeJBP8vDJQfip8UAAAAO
    AAAAAj+LvZa5AhwLP5bPEcVf/xgAAAAOAAAAAj+3KDkL3geAP8azYOQviyYAAAAOAAAAAj/C
    b7KO0yk6P9D56gw0/ZoAAAAOAAAAAj/uZmZmZmZmP+5mZmZmZmYAAAAOAAAAAsB+GcV7UbQI
    wIBteExc6aQAAAANAAAAAgAAAAMAAAAFAAAAEAAAAAIABAAJAAAAEUhseUVfSWdHK0hseUVf
    SWdBAAQACQAAABFIbHlFX0lnRytIbHlFX0lnQQAAAw0AAAACAAAAAQAAAAEAAAQCAAAAAQAE
    AAkAAAAGbGV2ZWxzAAAAEAAAAAUABAAJAAAAATEABAAJAAAAATIABAAJAAAAATMABAAJAAAA
    ATQABAAJAAAAATUAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAAAgAEAAkAAAAHb3JkZXJl
    ZAAEAAkAAAAGZmFjdG9yAAAA/gAABAIAAAABAAQACQAAAAVuYW1lcwAAABAAAAANAAQACQAA
    AAdTdHJhdHVtAAQACQAAAAljYXRjaG1lbnQABAAJAAAAAW4ABAAJAAAACWVzdC5zdGFydAAE
    AAkAAAAOaW5jaWRlbmNlLnJhdGUABAAJAAAAAlNFAAQACQAAAAZDSS5sd3IABAAJAAAABkNJ
    LnVwcgAEAAkAAAAIY292ZXJhZ2UABAAJAAAAB2xvZy5saWsABAAJAAAACml0ZXJhdGlvbnMA
    BAAJAAAADGFudGlnZW4uaXNvcwAEAAkAAAAUbmxtLmNvbnZlcmdlbmNlLmNvZGUAAAQCAAAA
    AQAEAAkAAAAJcm93Lm5hbWVzAAAADQAAAAIAAAABAAAAAgAABAIAAAL/AAAAEAAAAAQABAAJ
    AAAAGHN1bW1hcnkuc2Vyb2luY2lkZW5jZS5ieQAEAAkAAAAGdGJsX2RmAAQACQAAAAN0YmwA
    BAAJAAAACmRhdGEuZnJhbWUAAAQCAAAAAQAEAAkAAAALc3RyYXRhX3ZhcnMAAAAQAAAAAQAE
    AAkAAAAJY2F0Y2htZW50AAAEAgAAAAEABAAJAAAADGFudGlnZW5faXNvcwAAABAAAAACAAQA
    CQAAAAhIbHlFX0lnRwAEAAkAAAAISGx5RV9JZ0EAAAQCAAAAAQAEAAkAAAAGU3RyYXRhAAAA
    EAAAAAEABAAJAAAACWNhdGNobWVudAAABAIAAAABAAQACQAAAAlRdWFudGlsZXMAAAAOAAAA
    Aj+ZmZmZmZmgP+8zMzMzMzMAAAD+

