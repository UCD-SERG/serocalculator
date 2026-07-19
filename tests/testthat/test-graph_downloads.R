test_that(
  desc = "graph_downloads() returns a ggplot when gdl is installed",
  code = {
    skip_if_not_installed("gdl")
    # gdl delegates CRAN downloads to packageRank (an optional gdl
    # dependency); skip gracefully if it is unavailable.
    skip_if_not_installed("packageRank")
    skip_if_offline()
    graph_downloads() |>
      expect_s3_class("ggplot")
  }
)
