test_that("view_to_df works", {
  check_connected()
  file <- system.file("extdata", "gisdk", "testing", "toy_table.bin", package = "caliperR")
  view <- RunFunction("OpenTable", "test", "ffb", list(file, NA))
  df <- view_to_df(view)
  expect_equal(df$field_b, c("a", "b", NA, "d", "e", "g"))
  expect_equal(typeof(df$field_c), "character")
  expect_true(is.na(df$field_c[[1]]))
  RunFunction("CloseView", view)
})

test_that("df_to_view works", {
  check_connected()
  df <- data.frame("one" = c(1, 2), "two" = c(3, 4), "three" = c(5, 6))
  if ("r_view_1" %in% RunFunction("GetViews")[[1]]) RunFunction("CloseView", "r_view_1")
  view <- df_to_view(df)
  expect_equal(view, "r_view_1")
  expect_true(view %in% RunFunction("GetViews")[[1]])
  df2 <- view_to_df(view)
  expect_equal(df, df2)
  df$one <- 10
  view <- df_to_view(df, view)
  expect_equal(view, "r_view_1")
  df2 <- view_to_df(view)
  expect_equal(df, df2)
})

