test_that("RunMacro works", {
  check_connected()
  SetAlternateInterface(ui_path)
  expect_type(RunMacro("G30 Tutorial Folder"), "character")
  SetAlternateInterface()
})

test_that("Type conversion works", {
  check_connected()
  SetAlternateInterface(ui_path)
  expect_equal(RunMacro("return array"), list(1, 2))
  expect_equal(RunMacro("return vector"), c(1, 2))
  expect_equal(
    RunMacro("parse opts array", list("one" = 1)),
    "The first option name is one. The first option value is 1."
  )
  expect_equal(RunMacro("test nested vector")$test, c(1, 2, NA))
  expect_equal(caliper:::process_gisdk_args(1), 1)
  SetAlternateInterface(ui_path)
  expect_mapequal(
    RunMacro("return named array"),
    list("one" = 1, "two" = 2)
  )
  expect_setequal(
    RunMacro("return nested array"),
    list(list(1, 2), list(3, 4))
  )
  test <- list(
    "one" = list(NA)
  )
  expect_setequal(
    caliper:::process_gisdk_args(test),
    list(list("one", list(NA_complex_)))
  )
  SetAlternateInterface()
  expect_type(caliper:::convert_nulls_and_slashes(NA), "complex")
  expect_equal(caliper:::convert_nulls_and_slashes("a/b"), "a\\b")
})

test_that("RunFunction works", {
  check_connected()
  folder <- RunMacro("G30 Tutorial Folder")
  expect_equal(
    RunFunction(
      "OpenTable", "airports", "ffb", list(paste0(folder, "airports.bin"), NA)
    ),
    "airports"
  )
  expect_error(
    RunFunction("CreateObject"),
    "Use caliper::CreateObject()",
    fixed = TRUE
  )
})
