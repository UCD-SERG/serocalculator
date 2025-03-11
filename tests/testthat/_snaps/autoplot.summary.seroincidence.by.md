# error on plot type

    Code
      plot1 <- autoplot(est2sum, xvar = "ageCat", type = "whisker", dodge_width = 0.1,
        color_var = "catchment", CI = TRUE)
    Condition
      Error in `autoplot()`:
      ! Invalid plot `type` specified: "whisker".
      i Please choose either 'scatter' or 'bar'.

# error on incorrect yvar

    Code
      plot1 <- autoplot(est2sum, yvar = "fake", type = "bar", dodge_width = 0.1,
        color_var = "catchment", CI = TRUE)
    Condition
      Error in `strat_ests_barplot()`:
      ! The variable `fake` specified by argument `yvar` does not exist in `object`.
      Please choose a column that exists in `object`.

