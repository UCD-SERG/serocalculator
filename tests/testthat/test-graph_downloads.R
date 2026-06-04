test_that(
  desc = "graph_downloads() returns a ggplot when gdl is installed",
  code = {
    skip_if_not_installed("gdl")
    graph_downloads() |>
      expect_s3_class("ggplot")
  }
)
