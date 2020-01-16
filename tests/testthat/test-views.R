test_that("View conversion works", {
  check_connected()
  file <- system.file("extdata", "gisdk", "testing", "toy_table.bin", package = "caliper")
  view <- RunFunction("OpenTable", "test", "ffb", list(file, NA))
  df <- view_to_df(view)
  expect_equal(df$second, c("a", "b", NA, "c", "d"))
})
