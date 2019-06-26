# Store package-wide variables here rather than the global environment
caliper_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  try(disconnect(), silent = TRUE)
  try({
    connect(silent = TRUE)
    software <- get("CALIPER_SOFTWARE", envir = caliper_env)
  })
  if (exists("CALIPER_SOFTWARE", envir = caliper_env)) {
    packageStartupMessage("Connected to ", software)
  }
}

.onUnload <- function(libname, pkgname) {
  try(disconnect(), silent = TRUE)
}
