# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

#' R class that gives access to GISDK functions
#'
#' This class makes it easy to call any arbitrary GISDK function from R.
#'
#' @import R6
#' @export

CaliperR <- R6::R6Class("CaliperR",
  public = list(
    initialize = function() {
      if (!connected()) {
        connect()
      }
    },
    RunMacro = function(macro_name, ..., process_result = TRUE) {
      caliperR::RunMacro(macro_name, ..., process_result)
    },
    CreateObject = function(class_name, ...) {
      caliperR::CreateObject(class_name, ...)
    },
    SetAlternateInterface = function(ui_file = NULL) {
      caliperR::SetAlternateInterface(ui_file)
    },
    GetInterface = function() {
      caliperR::GetInterface()
    }
  )
)

#' S3 method for calling \code{CaliperR} object methods
#'
#' Makes \code{CaliperR} objects smarter about whether you are calling a
#' method from the R object or the underlying GISDK library.
#'
#' @details
#'
#' If \code{name} is a method of the R object, then it is executed. Otherwise,
#' it tries to execute a GISDK function of the same name.
#'
#' @param x A \code{CaliperR} object
#' @param name the method to dispatch
#' @export

`$.CaliperR` <- function(x, name, ..., process_result = TRUE) {
  args <- list(...)
  # If the name references an R method/attribute
  if (exists(name, envir = x)) {
    .subset2(x, name)
  # Otherwise, it must reference a GISDK function
  } else {
    function(...) {
      caliperR:::RunFunction(name, ...)
    }
  }
}

# dk <- connect()
# folder <- dk$RunMacro("G30 Tutorial Folder")
# view = dk$OpenTable("airports", "ffb", list(paste0(folder, "\\airports.bin")))
# dk$ShowArray(list("a", "b"))
# obj <- dk$CreateObject("NLM.Model")
