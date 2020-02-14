# Store package-wide variables here rather than the global environment
caliper_env <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  disconnect()
  try({
    connect(silent = TRUE)
    software <- get_package_variable("CALIPER_SOFTWARE")
  })
  if (exists("software")) {
    SetAlternateInterface(get_package_variable("GISDK_UTILS_UI"))
    p_info <- RunMacro("GetProgram")
    path <- p_info[[1]]
    software <- paste(p_info[[2]], p_info[[5]], "build", p_info[[4]],
                      paste0("(", p_info[[3]], ")"))
    SetAlternateInterface(get_package_variable("GISDK_UTILS_UI"))
    packageStartupMessage("Connected to ", software, "\n(", path, ")")
  }
}

.onDetach <- function(libpath) {
  disconnect()
}
