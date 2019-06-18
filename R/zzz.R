.onLoad <- function(libname, pkgname) {
  if (exists("CALIPER_DK", envir = .GlobalEnv)){
    remove("CALIPER_DK", envir = .GlobalEnv)
  }
  connect()
}
