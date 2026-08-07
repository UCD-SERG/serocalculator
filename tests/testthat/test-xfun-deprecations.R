test_that("deprecated xfun attribute access is not used in R source", {
  r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
  source_lines <- unlist(lapply(r_files, readLines), use.names = FALSE)

  expect_false(any(grepl("xfun::atrr\\(", source_lines)))
  expect_false(any(grepl("xfun::attr\\(", source_lines)))
  expect_false(any(grepl("\\battr\\([^)]*\\)(?!\\s*<-)", source_lines, perl = TRUE)))
})
