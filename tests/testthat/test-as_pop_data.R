test_that("results are consistent", {
   library(magrittr)
   xs_data <-
     serocalculator_example("example_pop_data.csv") %>%
     read.csv() %>%
     as_pop_data()

   expect_snapshot_value(xs_data, style = "serialize")
})
