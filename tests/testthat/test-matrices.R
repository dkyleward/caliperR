open_matrix <- function() {
  file <- system.file("extdata", "gisdk", "testing", "toy_matrix.mtx", package = "caliper")
  matrix <- RunFunction("OpenMatrix", file, NA)
  return(matrix)
}

test_that("matrix objects are created", {
  check_connected()
  matrix <- open_matrix()
  expect_type(matrix, "environment")
  expect_s4_class(matrix$handle, "COMIDispatch")
  expect_type(matrix$cores, "list")
  expect_s4_class(matrix$cores[[1]], "MatrixCurrency")
  expect_s4_class(matrix$cores[[1]]@com, "COMIDispatch")
  expect_equal(names(matrix$indices), c("row", "column"))
  expect_type(matrix$indices$row, "character")
})

test_that("matrix generics work", {
  check_connected()
  matrix <- open_matrix()
  expect_is(summary(matrix), "data.frame")
  expect_is(as.data.frame(matrix), "data.frame")
  expect_is(as.matrix(matrix), "list")
  expect_is(as.matrix(matrix$core_a), "matrix")
})

test_that("matrix indices work", {
  check_connected()
  matrix <- open_matrix()
  matrix$column_index <- "subset"
  c_labels <- RunFunction("GetMatrixColumnLabels", matrix$core_a)
  expect_equal(c_labels, list("1", "2", "3"))
  df <- as.data.frame(matrix)
  expect_equal(nrow(df), 15)
})
