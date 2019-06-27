# R6 approach to GISDK objects

# library(R6)

GisdkObject <- R6::R6Class("GisdkObject",
  public = list(
    ref = NULL,
    initialize = function(class_name, ...) {
      if (!connected()) stop("Not connected to Caliper software.")
      if (!is.character(class_name)) stop("'class_name' must be a string")
      self$ref <- RunMacro("create_object", NA, class_name, ...)
    },
    do = function(method_name) {
      if (!is.character(method_name)) stop("'method_name' must be a string")
      print(method_name)
    }
  )
)

# obj <- GisdkObject$new("NLM.Model")
# obj$do("test2")
