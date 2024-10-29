test_that("df_to_array() produces consistent results", {
  library(dplyr)
  library(tidyr)
  df <- iris |>
    tidyr::pivot_longer(
      names_to = "parameter",
      cols = c(
        "Sepal.Length",
        "Sepal.Width",
        "Petal.Width",
        "Petal.Length"
      )
    ) |>
    mutate(parameter = factor(parameter, levels = unique(parameter)))
  arr <- df |>
    serocalculator:::df_to_array(dim_var_names = c("parameter", "Species"))
  arr |> expect_snapshot_value(style = "serialize")
})
