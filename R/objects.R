#' R class representing GISDK objects
#'
#' @import R6
#' @export
#' @examples
#' \dontrun{
#' # object creation
#' obj <- GisdkObject$new("NLM.Model")
#'
#' # get info about the GISDK object
#' obj$GetClassInfo()
#'
#' # run one of the object's methods
#' obj$Clear()
#'
#' # object methods can be chained
#' obj$
#'  Clear()$
#'  Read()
#' }

GisdkObject <- R6::R6Class("GisdkObject",
  public = list(
    ref = NULL,
    info = NULL,
    initialize = function(class_name, ...) {
      if (!connected()) stop("Not connected to Caliper software.")
      if (!is.character(class_name)) stop("'class_name' must be a string")
      tryCatch(
        self$ref <- RunMacro("create_object", NA, class_name, ...),
        error = function(e) {
          e$message <- paste0("Object '", class_name, "' could not be created")
          stop(e)
        }
      )
      flat_list <- self$GetClassInfo()
      names <- c("ClassName", "FieldNames", "MethodNames")
      positions <- which(flat_list %in% names)
      pattern <- rep(names, times = diff(c(positions, length(flat_list) + 1)))
      result <- split(flat_list, pattern)
      self$info <- lapply(result, function(x) x[2:length(x)])
    },
    GetClassInfo = function() {
      RunMacro("get_class_info", self$ref)
    },
    apply_method = function(method, ...) {
      stopifnot(is.character(method))
      args <- list("apply_method", self$ref, method, ...)
      tryCatch(
        do.call(RunMacro, args),
        error = function(e) {
          e$message <- paste0(method, "() failed.")
          stop(e)
        }
      )
      invisible(self)
    }#,
    # get_attribute = function(attribute) {
    #   stopifnot(is.character(attribute))
    #   args <- list("get_attribute", self$ref, attribute)
    #   tryCatch(
    #     value <- do.call(RunMacro, args),
    #     error = function(e) {
    #       e$message <- paste0("Setting ", attribute, " failed.")
    #       stop(e)
    #     }
    #   )
    #   process_gisdk_result(value)
    # },
    # set_attribute = function(attribute, ...) {
    #   stopifnot(is.character(attribute))
    #   input_args <- process_gisdk_args(...)
    #   args <- list("set_attribute", self$ref, attribute, input_args)
    #   tryCatch(
    #     do.call(RunMacro, args),
    #     error = function(e) {
    #       e$message <- paste0("Setting ", attribute, " failed.")
    #       stop(e)
    #     }
    #   )
    #   invisible(self)
    # }
  )
)

#' S3 generic for calling GISDK object methods
#'
#' This function allows you to use \code{obj$method_name(...)} to dispatch
#' arbitrary method names on GISDK objects. See \code{\link{GisdkObject}}.
#'
#' @param x A \code{GisdkObject}
#' @param name the method to dispatch
#' @export

`$.GisdkObject` <- function(x, name) {
  if (exists(name, envir = x)) {
    .subset2(x, name)
  } else {
    function(...) {
      .subset2(x, "apply_method")(name, ...)
    }
  }
}

# `$<-.GisdkObject` <- function(x, name) {
#   if (exists(name, envir = x)) {
#     .subset2(x, name)
#   } else {
#     function(...) {
#       .subset2(x, "apply_method")(name, ...)
#     }
#   }
# }

# obj <- GisdkObject$new("G30 Progress Bar")
# obj$SetMessage("test")
# obj$Step()
# obj$Step
