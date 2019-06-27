# Store package-wide variables here rather than the global environment
caliper_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  try(disconnect(), silent = TRUE)
  try({
    connect(silent = TRUE)
    software <- get_package_variable("CALIPER_SOFTWARE")
  })
  if (exists("software")) {
    packageStartupMessage("Connected to ", software)
  }
}

.onUnload <- function(libname, pkgname) {
  try(disconnect(), silent = TRUE)
}
