.onLoad <- function(libname, pkgname) {
  try(disconnect(), silent = TRUE)
  try({
    connect()
    software <- get("CALIPER_SOFTWARE", envir = .GlobalEnv)
  })
  if (exists("CALIPER_SOFTWARE", envir = .GlobalEnv)) {
    packageStartupMessage("Connected to ", software)
  }
}

.onUnload <- function(libname, pkgname) {
  try(disconnect(), silent = TRUE)
}
