test_that("view_to_df works", {
  check_connected()
  file <- system.file("extdata", "gisdk", "testing", "toy_table.bin", package = "caliper")
  view <- RunFunction("OpenTable", "test", "ffb", list(file, NA))
  df <- view_to_df(view)
  expect_equal(df$second, c("a", "b", NA, "c", "d"))
})

test_that("df_to_view works", {
  check_connected()
  df <- data.frame("one" = c(1, 2), "two" = c(3, 4), "three" = c(5, 6))
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

test_that("reading a bin file works over COM", {
  check_connected()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliper"
  )
  df <- read_bin(bin_file)
  dnames <- read_bin(bin_file, returnDnames = TRUE)
  expect_equal(df$field_b[[1]], "a") # white space removed
  expect_true(is.na(df$field_b[[3]])) # convert null characters to NA
  expect_equal(typeof(df[[3]]), 'character') # preserve type for null fields
  expect_equal(typeof(df[[5]]), 'double') # preserve type for null fields
  expect_equal(nrow(df), 6) # make sure the deleted record isn't read in.
  expect_equal(Hmisc::label(df)[[1]], "first field") # field descriptions
})

test_that("reading a bin file works without COM", {
  disconnect()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliper"
  )
  df <- read_bin(bin_file)
  dnames <- read_bin(bin_file, returnDnames = TRUE)
  expect_equal(df$second[[1]], "a") # white space removed
  expect_true(is.na(df$second[[3]])) # convert null characters to NA
  expect_equal(typeof(df[[3]]), 'character') # preserve type for null fields
  expect_equal(typeof(df[[5]]), 'double') # preserve type for null fields
  expect_equal(nrow(df), 6) # make sure the deleted record isn't read in.
  expect_equal(Hmisc::label(df)[[1]], "first field") # field descriptions
  try(connect(), silent = TRUE)
})

testh_that("reading with and without COM give same data", {
  check_connected()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliper"
  )
  df1 <- read_bin(bin_file)
  disconnect()
  df2 <- read_bin(bin_file)
  expect_true(all(df2 == df, na.rm = TRUE))
  expect_true(all(unlist(lapply(df1, typeof)) == unlist(lapply(df2, typeof))))
  try(connect(), silent = TRUE)
})

test_that("writing a bin file works without COM", {
  disconnect()
  bin_file <- system.file(
    "extdata", "gisdk", "testing", "toy_table.bin", package = "caliper"
  )
  df1 <- read_bin(bin_file)
  dnames <- read_bin(bin_file, returnDnames = TRUE)
  temp_bin <- tempfile(fileext = ".bin")
  write_bin(df1, temp_bin, dnames = dnames)
  df2 <- read_bin(temp_bin)
  expect_true(all(unlist(lapply(df1, typeof)) == unlist(lapply(df2, typeof))))
  temp_dcb <- gsub("\\.bin", "\\.dcb", temp_bin)
  con <- file(temp_dcb, method = "r")
  on.exit(close(con))
  expect_equal(
    readLines(con)[3],
    "\"first\",I,1,4,0,10,0,,\"\",\"first field\",,\"Copy\",\"field_a\""
  )
  close(con)
  try(connect(), silent = TRUE)
})
