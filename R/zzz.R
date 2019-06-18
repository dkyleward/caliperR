.onLoad <- function(libname, pkgname) {
  if (exists("caliper_dk", envir = .GlobalEnv)){
    remove("caliper_dk", envir = .GlobalEnv)
  }
  connect()
}
