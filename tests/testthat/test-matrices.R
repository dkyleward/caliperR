open_matrix <- function() {
  folder <- RunMacro("G30 Tutorial Folder")
  file <- paste0(folder, "Accessibility Skim BusWlk.mtx")
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
