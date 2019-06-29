#' Creates an R object that reprents a GISDK object
#'
#' A simple function that merely abstracts Rs creation language for R6 objects
#' to make it look more like GISDK. See \code{\link{CaliperClass}} for details
#' on the class and how to work with objects.
#'
#' @export
#' @param class_name The class name of the GISDK object to create.
#' @param ... Any additional arguments to use during GISDK object creation.
#' @examples
#' \dontrun{
#' obj <- CreateObject("NLM.Model")
#' }

CreateObject <- function(class_name, ...) {
  CaliperClass$new(class_name, ...)
}

#' R class representing GISDK objects
#'
#' This class simplifies working with GISDK objects over COM from R.
#'
#' @inheritParams CreateObject
#'
#' @details
#'
#' @section Overview:
#'
#' When created, an object of this class contains a COM pointer to a sister
#' object in Caliper software. In addition to this reference, contained in the
#' \code{ref} field, there are several other fields and methods to facilitate
#' object manipulation. Finally, the \code{$} operator has been overloaded for
#' this class to make code easier to write.
#'
#' @section Object fields:
#'
#' A \code{CaliperClass} object has the following fields/attributes of interest:
#' \describe{
#'   \item{g_class_name}{
#'     The class name of the underlying GISDK object (e.g. "NLM.Model").
#'   }
#'   \item{ref}{
#'     This is a COM pointer and represents the object created in a Caliper
#'     program.
#'   }
#'   \item{info}{
#'     This is a simple R list that can be used to see the fields and methods
#'     of the underlying GISDK object.
#'   }
#' }
#'
#' @section Object methods:
#'
#' A \code{CaliperClass} object has several methods. While they are listed
#' below for completeness, you likely won't need to use the methods directly
#' (see examples).
#'
#' \describe{
#'   \item{g_apply_method}{
#'     Dispatches one of the GISDK object's methods
#'   }
#'   \item{g_get_attribute(attribute)}{
#'     Gets the value of a named attribute of the GISDK object
#'   }
#'   \item{g_set_attribute(attribute)}{
#'     Sets the value of a named attribute of the GISDK object
#'   }
#' }
#'
#' @import R6
#' @export
#' @examples
#' \dontrun{
#' # object creation (easier to use caliper::CreateObject())
#' obj <- CaliperClass$new("NLM.Model")
#'
#' # get info about the GISDK object fields and methods
#' obj$info
#'
#' # set an attribute/field label of the GISDK object
#' # (Does not modify the R object in any way)
#' obj$Label <- "a logit model"
#'
#' # run one of the GISDK object's methods: Clear()
#' # (Does not modify the R object in any way)
#' obj$Clear()
#'
#' # object methods can be chained
#' obj$
#'  Clear()$
#'  Read()
#' }

CaliperClass <- R6::R6Class("CaliperClass",
  public = list(
    g_class_name = NULL,
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
      self$g_class_name <- class_name
      flat_info_list <- RunMacro("get_class_info", self$ref)
      names <- c("ClassName", "FieldNames", "MethodNames")
      pos <- which(flat_info_list %in% names)
      pattern <- rep(names, times = diff(c(pos, length(flat_info_list) + 1)))
      result <- split(flat_info_list, pattern)
      self$info <- lapply(result, function(x) x[2:length(x)])
    },
    g_apply_method = function(method, ...) {
      stopifnot(is.character(method))
      args <- list(...)
      args <- caliper:::process_gisdk_args(args)
      args <- list("apply_method", self$ref, method, args)
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

#' S3 generic for calling \code{CaliperClass} object methods
#'
#' Makes \code{CaliperClass} objects smarter about whether you are calling a
#' method from the R object or the underlying GISDK object over COM.
#'
#' @details
#'
#' If \code{name} is an attribute of the R object (like \code{$info}), then
#' the value of that attribute is returned. Otherwise, it looks into the fields
#' and methods of the underlying GISDK object to determine what to do.
#'
#' @param x A \code{CaliperClass} object
#' @param name the method to dispatch
#' @export

`$.CaliperClass` <- function(x, name) {
  info <- .subset2(x, "info")
  if (exists(name, envir = x)) {
    .subset2(x, name)
  } else if (name %in% info$MethodNames) {
    function(...) {
      .subset2(x, "g_apply_method")(name, ...)
    }
  } else if (name %in% info$FieldNames) {
      .subset2(x, "g_get_attribute")(name)
  } else {
    stop(paste0(name, " not found in R or GISDK objects"))
  }
}

#' S3 generic for assigning \code{CaliperClass} object attributes
#'
#' Makes \code{CaliperClass} objects smarter about whether you are assigning a
#' value to the R object or the underlying GISDK object over COM.
#'
#' If \code{name} is an attribute of the R object (like \code{$info}), then
#' the value is assigned to that attribute. Otherwise, it looks into the GISDK
#' object attributes and will set that value.
#'
#' @param x A \code{CaliperClass} object
#' @param name the attribute to assign
#' @param value the value to be assigned
#' @export

`$<-.CaliperClass` <- function(x, name, value) {
  if (exists(name, envir = x)) {
    assign(name, value, envir = x)
  } else if (name %in% x$info$FieldNames) {
    .subset2(x, "g_set_attribute")(name, value)
  } else {
    stop(paste0("'", name, "' not found in R or GISDK objects"))
  }
  invisible(x)
}
