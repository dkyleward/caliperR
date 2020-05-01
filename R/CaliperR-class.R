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
    }
  )
)


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

# dk <- CaliperR$new()
# folder <- dk$RunMacro("G30 Tutorial Folder")
# view = dk$OpenTable("airports", "ffb", list(paste0(folder, "\\airports.bin")))
# dk$ShowArray(list("a", "b"))
# obj <- dk$CreateObject("NLM.Model")
