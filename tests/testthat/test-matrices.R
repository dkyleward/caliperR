open_matrix <- function() {
  file <- system.file("extdata", "gisdk", "toy_matrix.mtx", package = "caliper")
  matrix <- RunFunction("OpenMatrix", file, NA)
  return(matrix)
}

test_that("matrix objects are created", {
  check_connected()
  matrix <- open_matrix()
  expect_type(matrix, "environment")
  expect_s4_class(matrix$handle, "COMIDispatch")
  expect_type(matrix$cores, "list")
  expect_s4_class(matrix$cores[[1]], "COMIDispatch")
})

test_that("matrix generics work", {
  check_connected()
  matrix <- open_matrix()
  expect_is(summary(matrix), "data.frame")
  expect_is(as.data.frame(matrix), "data.frame")
  expect_is(as.matrix(matrix), "matrix")
})

test_that("matrix indices work", {
  check_connected()
  matrix <- open_matrix()
  matrix$column_index <- "subset"
  c_labels <- RunFunction("GetMatrixColumnLabels", matrix$cores$`core a`)
  expect_equal(c_labels, c("1", "2", "3"))
  df <- as.data.frame(matrix)
  expect_equal(nrow(df), 15)
})
