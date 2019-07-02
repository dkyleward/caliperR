# Store package-wide variables here rather than the global environment
caliper_env <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  disconnect()
  try({
    connect(silent = TRUE)
    software <- get_package_variable("CALIPER_SOFTWARE")
  })
  if (exists("software")) {
    packageStartupMessage("Connected to ", software)
  }
}

.onDetach <- function(libpath) {
  disconnect()
}
