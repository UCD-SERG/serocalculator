# est_seroincidence() produces expected results for typhoid data

    Code
      summary(typhoid_results, coverage = 0.95)
    Output
      # A tibble: 1 x 10
        est.start incidence.rate     SE CI.lwr CI.upr coverage log.lik iterations
            <dbl>          <dbl>  <dbl>  <dbl>  <dbl>    <dbl>   <dbl>      <int>
      1       0.1          0.166 0.0178  0.135  0.205     0.95   -524.          5
      # i 2 more variables: antigen.isos <chr>, nlm.convergence.code <ord>

---

    structure(list(minimum = 523.575044823023, estimate = -1.7955958453869, 
        gradient = 3.60891331241403e-06, hessian = structure(86.991906300701, dim = c(1L, 
        1L)), code = 1L, iterations = 5L), class = c("seroincidence", 
    "list"), lambda_start = 0.1, antigen_isos = c("HlyE_IgG", "HlyE_IgA"
    ))

# verbose output is consistent

    Code
      est_seroincidence(pop_data = sees_pop_data_pk_100, sr_param = typhoid_curves_nostrat_100,
        noise_param = example_noise_params_pk, antigen_isos = c("HlyE_IgG",
          "HlyE_IgA"), verbose = TRUE)
    Message
      i nrow(sr_params) = 200
      Initial negative log-likelihood: 533.379886031329
      about to call `nlm()`
    Output
      iteration = 0
      Step:
      [1] 0
      Parameter:
      [1] -2.3025851
      Function Value
      [1] 533.37989
      Gradient:
      [1] -35.939944
      
      iteration = 1
      Step:
      [1] 0.33515619
      Parameter:
      [1] -1.9674289
      Function Value
      [1] 524.8067
      Gradient:
      [1] -14.024947
      
      iteration = 2
      Step:
      [1] 0.21449
      Parameter:
      [1] -1.7529389
      Function Value
      [1] 523.65497
      Gradient:
      [1] 3.7657315
      
      iteration = 3
      Step:
      [1] -0.04540084
      Parameter:
      [1] -1.7983397
      Function Value
      [1] 523.57537
      Gradient:
      [1] -0.23844672
      
      iteration = 4
      Step:
      [1] 0.0027035963
      Parameter:
      [1] -1.7956361
      Function Value
      [1] 523.57504
      Gradient:
      [1] -0.0035022135
      
      iteration = 5
      Parameter:
      [1] -1.7955958
      Function Value
      [1] 523.57504
      Gradient:
      [1] 3.6089133e-06
      
      Relative gradient close to zero.
      Current iterate is probably solution.
      
      `seroincidence` object estimated given the following setup:
      a) `antigen_isos`:  HlyE_IgG, HlyE_IgA 
      b) `lambda_start`:  0.1 
      Call the `summary()` function to obtain output results.
      Call the `autoplot()` function to graph the log-likelihood curve.

