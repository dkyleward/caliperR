library(testthat)
library(caliper)

check_connected <- function() {
  if (!connected()) {
    skip("Not connected to Caliper software")
  }
}

# unzip the compiled UI file
zip_file <- system.file("extdata", "gisdk", "my_ui.zip", package = "caliper")
tempdir <- tempdir()
unzip(zip_file, exdir = tempdir, setTimes = TRUE)
ui_path <- file.path(tempdir, "my_ui.dbd")

connect()
test_check("caliper")

# for interactive testing:
# test_dir(path = "tests/testthat")
