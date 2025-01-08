test_that("`serocalculator_example()` works", {

  path1 = serocalculator_example("example_pop_data.csv")
  path2 = fs::path_package(package = "serocalculator",
                           "extdata/example_pop_data.csv")
  expect_equal(path1, path2)


  files1 = serocalculator_example()
  files2 = "example_pop_data.csv"

  expect_equal(files1, files2)

})
