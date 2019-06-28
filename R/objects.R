#' R class representing GISDK objects
#'
#' This class simplifies working with GISDK objects over COM from R.
#'
#' @import R6
#' @export
#' @examples
#' \dontrun{
#' # object creation
#' obj <- GisdkObject$new("NLM.Model")
#'
#' # get info about the GISDK object
#' obj$g_info
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
    g_class_name = NULL,
    ref = NULL,
    g_info = NULL,
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
      self$g_class_name <- class_name
      flat_info_list <- RunMacro("get_class_info", self$ref)
      names <- c("ClassName", "FieldNames", "MethodNames")
      pos <- which(flat_info_list %in% names)
      pattern <- rep(names, times = diff(c(pos, length(flat_info_list) + 1)))
      result <- split(flat_info_list, pattern)
      self$g_info <- lapply(result, function(x) x[2:length(x)])
    },
    g_apply_method = function(method, ...) {
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
    },
    g_get_attribute = function(attribute) {
      stopifnot(is.character(attribute))
      args <- list("get_attribute", self$ref, attribute)
      tryCatch(
        value <- do.call(RunMacro, args),
        error = function(e) {
          e$message <- paste0("Setting ", attribute, " failed.")
          stop(e)
        }
      )
      caliper:::process_gisdk_result(value)
    },
    g_set_attribute = function(attribute, value) {
      stopifnot(is.character(attribute))
      value <- caliper:::process_gisdk_args(value)
      args <- list("set_attribute", self$ref, attribute, value)
      tryCatch(
        do.call(RunMacro, args),
        error = function(e) {
          e$message <- paste0("Setting ", attribute, " failed.")
          stop(e)
        }
      )
      invisible(self)
    }
  )
)

#' S3 generic for calling GisdkObject methods
#'
#' Makes GisdkObjects smarter about whether you are calling a method from the R
#' object or the underlying GISDK object over COM.
#'
#' @param x A \code{GisdkObject}
#' @param name the method to dispatch
#' @export

`$.GisdkObject` <- function(x, name) {
  if (exists(name, envir = x)) {
    .subset2(x, name)
  } else if (name %in% x$g_info$MethodNames) {
    function(...) {
      .subset2(x, "g_apply_method")(name, ...)
    }
  } else if (name %in% x$g_info$FieldNames) {
    function(name) {
      .subset2(x, "g_get_attribute")(name)
    }
  }
}

`$<-.GisdkObject` <- function(x, name, value) {
  if (exists(name, envir = x)) {
    assign(name, value, envir = x)
  } else if (name %in% x$g_info$FieldNames) {
    .subset2(x, "g_set_attribute")(name, value)
  } else {
    stop(paste0("'", name, "' not found in R or GISDK objects"))
  }
  invisible(x)
}

#' S3 generic for assigning GisdkObject attributes
#'
#' Makes GisdkObjects smarter about whether you are assigning a value to the R
#' object or the underlying GISDK object over COM.
#'
#' @param x A \code{GisdkObject}
#' @param name the attribute to assign
#' @param value the value to be assigned
#' @export

# `[<-.GisdkObject` <- function(x, name, value) {
#   if (exists(name, envir = x)) {
#     x$name <- value
#   } else if (name %in% x$g_info$FieldNames) {
#     .subset2(x, "g_set_attribute")(name, value)
#   }
# }

obj <- GisdkObject$new("G30 Progress Bar")
# obj$g_get_attribute("CurrentStep") # works
# obj$g_set_attribute("CurrentStep", 1) # works
# obj["CurrentStep"] <- 1 # works
obj$CurrentStep <- 2 # works
obj$CurrentStep
# obj$SetMessage("test")
# obj$Message

