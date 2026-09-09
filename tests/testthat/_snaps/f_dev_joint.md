# multi-biomarker joint log-likelihood is not the sum of marginals

    -507.270916221597

# `est_seroincidence(method = 'joint')` fits and summarizes

    list(500.230223335819, -1.77958502107846, 2.41481155155645e-05, 
        structure(59.8376402649724, dim = c(1L, 1L)), 1L, 5L)

---

    Code
      print(est_joint)
    Output
      `seroincidence` object estimated given the following setup:
      a) `antigen_isos`:  HlyE_IgA, HlyE_IgG 
      b) `lambda_start`:  0.1 
      c) `method`:  joint 
      Call the `summary()` function to obtain output results.
      Call the `autoplot()` function to graph the log-likelihood curve.

# the joint estimator recovers a simulated incidence rate

    structure(list(incidence.rate = 0.161994191989858, SE = 0.0187081648809979), row.names = c(NA, 
    -1L), class = c("summary.seroincidence", "tbl_df", "tbl", "data.frame"
    ))

