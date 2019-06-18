.onLoad <- function(libname, pkgname) {
  if (exists("CALIPER_DK", envir = .GlobalEnv)){
    remove("CALIPER_DK", envir = .GlobalEnv)
  }
  if (exists("CALIPER_SOFTWARE", envir = .GlobalEnv)){
    remove("CALIPER_SOFTWARE", envir = .GlobalEnv)
  }
  if (exists("CALIPER_UI", envir = .GlobalEnv)){
    remove("CALIPER_UI", envir = .GlobalEnv)
  }
  try({
    connect()
    software <- get("CALIPER_SOFTWARE", envir = .GlobalEnv)
  })
  if (exists("CALIPER_SOFTWARE", envir = .GlobalEnv)) {
    packageStartupMessage("Connected to ", software)
  }
}
