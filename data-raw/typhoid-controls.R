library(readr)

tc = read_csv("inst/extdata/typhoidcontrols.csv")
tc |>
  summarize(
    .by = c(, antigen_iso),
    nu = quantile(
      elisa,
      p = .95,

    )
  )

library(ggplot2)
ggplot(tc, aes(x = Age, y = elisa, col = antigen_iso)) +
  geom_point() +
  geom_smooth()

typhoid_controls = tc

usethis::use_data(typhoid_controls, overwrite = TRUE)
