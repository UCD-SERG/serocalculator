# `est.incidence.by()` produces consistent results for typhoid data

    Code
      strat_ests

---

    structure(list("Stratum 1" = structure(list(minimum = 269.456336163178, 
        estimate = -1.9659114149187, gradient = -9.97551535545146e-06, 
        hessian = structure(42.1287040808331, dim = c(1L, 1L)), code = 1L, 
        iterations = 4L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA")), "Stratum 2" = structure(list(minimum = 252.757931730411, 
        estimate = -1.61127190941552, gradient = 3.06923829056525e-06, 
        hessian = structure(44.1077020241307, dim = c(1L, 1L)), code = 1L, 
        iterations = 5L), class = c("seroincidence", "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", 
    "HlyE_IgA"))), antigen_isos = c("HlyE_IgG", "HlyE_IgA"), Strata = structure(list(
        Stratum = c("Stratum 1", "Stratum 2"), catchment = c("aku", 
        "kgh"), n = c(53L, 47L)), row.names = c(NA, -2L), class = c("tbl_df", 
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
      1 Stratu~ aku          53       0.1          0.140 0.0216  0.104  0.189     0.95
      2 Stratu~ kgh          47       0.1          0.200 0.0301  0.149  0.268     0.95
      # i 4 more variables: log.lik <dbl>, iterations <int>, antigen.isos <chr>,
      #   nlm.convergence.code <ord>

---

    WAoAAAACAAQEAgACAwAAAAMTAAAADQAAABAAAAACAAQACQAAAAlTdHJhdHVtIDEABAAJAAAA
    CVN0cmF0dW0gMgAAABAAAAACAAQACQAAAANha3UABAAJAAAAA2tnaAAAAA0AAAACAAAANQAA
    AC8AAAAOAAAAAj+5mZmZmZmaP7mZmZmZmZoAAAAOAAAAAj/B7HG3u31GP8mNl3vEzx8AAAAO
    AAAAAj+WF3CeJOYiP57H0YBnEdYAAAAOAAAAAj+6gQyGCIncP8MF3OtDSO8AAAAOAAAAAj/I
    PfHsmRXgP9EpmJ2X/6MAAAAOAAAAAj/uZmZmZmZmP+5mZmZmZmYAAAAOAAAAAsBw100nJg1N
    wG+YQPoLVvAAAAANAAAAAgAAAAQAAAAFAAAAEAAAAAIABAAJAAAAEUhseUVfSWdHK0hseUVf
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

