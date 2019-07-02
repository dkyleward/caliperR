check_connected <- function() {
  if (!connected()) {
    skip("Not connected to Caliper software")
  }
}

test_that("Objects work", {
  check_connected()
  expect_type(CreateObject("NLM.Model"), "environment")
  obj <- CreateObject("NLM.Model")
  obj$info
  expect_type(obj$info, "list")
  expect_type(obj$info$MethodNames[1], "character")
  expect_error(obj$Labelz, "Labelz not found in R or GISDK objects")
  obj$Label <- "A logit model"
  expect_equal(obj$Label, "A logit model")
  obj$Clear()
  expect_null(obj$Label, "A logit model")
})
