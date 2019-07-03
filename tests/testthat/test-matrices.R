test_that("matrix objects are created", {
  check_connected()
  folder <- RunMacro("G30 Tutorial Folder")
  file <- paste0(folder, "Accessibility Skim BusWlk.mtx")
  matrix <- create_matrix(file)
  expect_type(matrix, "list")
  expect_s4_class(matrix$ref, "COMIDispatch")
  expect_type(matrix$cores, "list")
  expect_s4_class(matrix$cores[[1]], "COMIDispatch")
})

# test_that("matrices work", {
#   check_connected()
#   folder <- RunMacro("G30 Tutorial Folder")
#   file <- paste0(folder, "Accessibility Skim BusWlk.mtx")
#   matrix <- create_matrix(file)
#   x <- matrix$currencies$`In-Vehicle Time`
#   temp_file <- tempfile(fileext = ".omx")
#   com_obj <- RunFunction(
#     "CopyMatrix", x[[1]],
#     list(
#       "File Name" = temp_file,
#       "Label" = "test",
#       "OMX" = "true"
#     )
#   )
#   RunFunction("GetMatrixCoreNames", com_obj)
#   remove(matrix)
#   remove(x)
#   remove(com_obj)
# })
