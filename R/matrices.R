#' S3 method for converting a \code{CaliperMatrix} into a \code{data.frame}
#'
#' @param x \code{CaliperMatrix}
#' @param row.names See \code{as.data.frame}
#' @param optional See \code{as.data.frame}
#' @param ... additional arguments passed to \code{as.data.frame}
#' @return Returns a \code{data.frame} with \code{from} and \code{to} columns
#'   along with a column for each core of the matrix. The \code{data.frame}
#'   respects the current row and column indices.
#' @import data.table
#' @export

as.data.frame.CaliperMatrix <- function(x, row.names = NULL,
                                        optional = FALSE, ...) {

  temp_file <- tempfile(fileext = ".csv")
  core_names <- names(x$cores)
  RunFunction(
    "CreateTableFromMatrix", x$handle, temp_file, "CSV",
    list(Complete = TRUE)
  )
  df <- data.table::fread(temp_file, header = FALSE)
  colnames(df) <- c("from", "to", core_names)
  setDF(df, rownames = row.names)

  # CreateTableFromMatrix exports all the data regardless of which index
  # is active. Filter the from/to columns to match the active index.
  c_labels <- RunFunction("GetMatrixColumnLabels", x$cores[[1]])
  c_labels <- as.numeric(c_labels)
  r_labels <- RunFunction("GetMatrixRowLabels", x$cores[[1]])
  r_labels <- as.numeric(r_labels)
  df <- df[df$from %in% r_labels & df$to %in% c_labels, ]

  return(df)
}

#' S3 method for bringing caliper matrices into R matrix format
#'
#' Converts a Caliper matrix into R format allowing it to be worked on in the R
#' environment.
#'
#' By default, the first core of the Caliper matrix is converted into an R
#' matrix. To select a different one, use the \code{core} argument like so:
#' \code{as.matrix(x, core = "second core")}.
#'
#' @param x A \code{CaliperMatrix} object
#' @param ... Additional arguments passed to \code{as.matrix}.
#' @param core \code{string} Optional name of core to convert. Defaults to
#'   first core.
#' @return an R matrix
#' @export

as.matrix.CaliperMatrix <- function(x, ..., core = NULL) {

  # Argument checking
  if (!is.null(core)) {
    stopifnot(core %in% names(x$cores))
  } else {
    core <- names(x$cores)[1]
  }

  tbl <- as.data.frame.CaliperMatrix(x)

  tbl$from <- factor(x = tbl$from, levels = unique(tbl$from))
  tbl$to <- factor(x = tbl$to, levels = unique(tbl$to))

  result <- matrix(
    nrow = nlevels(tbl$from),
    ncol = nlevels(tbl$to),
    dimnames = list(levels(tbl$from), levels(tbl$to))
  )
  result[cbind(tbl$from, tbl$to)] <- tbl[, core]

  return(result)
}

#' S3 method for summarizing a \code{CaliperMatrix}
#'
#' Statistics return include things like sum, mean, min, etc.
#'
#' @param object A \code{CaliperMatrix} object.
#' @param ... Additional arguments passed to the \code{summary} generic
#'   function.
#' @return A \code{data.frame} of summary statistics. These summary statistics
#'   are always for the full Caliper matrix and do not respect the current
#'   index.
#' @import data.table
#' @export

summary.CaliperMatrix <- function(object, ...) {

  # Argument checking
  stopifnot("CaliperMatrix" %in% class(object))

  stats <- RunFunction("MatrixStatistics", object$handle, NA)
  list_of_rows <- mapply(
    function(x, name) {
      df <- as.data.frame(x)
      df$Core <- name
      return(df)
    },
    x = stats,
    name = names(stats),
    SIMPLIFY = FALSE
  )

  df <- data.table::rbindlist(list_of_rows)
  setcolorder(df, "Core")
  return(as.data.frame(df))
}

#' CaliperMatrix class
#'
#' This class makes it easier to interact with Caliper matrices in R.
#'
#' @section Object fields:
#'
#' A \code{CaliperMatrix} object has the following fields/attributes of interest:
#' \describe{
#'   \item{handle}{
#'     This is a COM pointer and represents the object in Caliper Software.
#'   }
#'   \item{cores}{
#'     This is a named list of cores. Each element contains a COM pointer to
#'     a matrix currency in Caliper software.
#'   }
#'   \item{indices}{
#'     This lists the row and column indices available for the matrix.
#'   }
#'   \item{row_index}{
#'     This can be used to retrieve or set the current row index.
#'   }
#'   \item{column_index}{
#'     This can be used to retrieve or set the current column index.
#'   }
#' }
#'
#' See the vignette "Using caliper" for more details on interacting with
#' matrices. This includes info on S3 methods for bringing them into R formats
#' like \code{data.frames} and \code{matrices}.
#'
#' @import R6
#' @export

CaliperMatrix <- R6::R6Class(
  "CaliperMatrix",
  public = list(
    handle = NULL,
    cores = NULL,
    indices = NULL,
    initialize = function (matrix) {
      if (typeof(matrix) == "character") {
        stopifnot(file.exists(matrix))
        self$handle <- RunFunction("OpenMatrix", matrix, NA)
      }
      if (class(matrix) == "COMIDispatch") {
        self$handle <- matrix
      }
      base_indices <- RunFunction("GetMatrixBaseIndex", self$handle)
      private$current_row_index <- base_indices[[1]]
      private$current_column_index <- base_indices[[2]]
      self$create_matrix_cores()
      indices <- RunFunction(
        "GetMatrixIndexNames", self$handle, process_result = FALSE
      )
      indices <- lapply(indices, function(x) unlist(x))
      indices <- setNames(indices, c("row", "column"))
      self$indices <- indices
    },
    create_matrix_cores = function () {
      result <- RunFunction(
        "CreateMatrixCurrencies", self$handle, private$current_row_index,
        private$current_column_index, NA
      )
      self$cores <- result
      invisible(self)
    }
  ),
  active = list(
    row_index = function (name) {
      if (missing(name)) return(private$current_row_index)
      if (!(name %in% self$indices$row)) {
        stop(paste0(
          "Name must be one of ", paste(self$indices$row, collapse = ", ")
        ))
      }
      self$cores <- NULL
      private$current_row_index <- name
      self$create_matrix_cores()
    },
    column_index = function (name) {
      if (missing(name)) return(private$current_column_index)
      if (!(name %in% self$indices$column)) {
        stop(paste0(
          "Name must be one of ", paste(self$indices$column, collapse = ", ")
        ))
      }
      self$cores <- NULL
      private$current_column_index <- name
      self$create_matrix_cores()
    }
  ),
  private = list(
    current_row_index = NULL,
    current_column_index = NULL
  )
)

#' S3 method for calling \code{CaliperMatrix} object attributes
#'
#' Makes it easier to call a matrix core directly using \code{matrix$core_name}.
#'
#' @details
#'
#' If \code{name} is an attribute of the R object (like \code{$info}), then
#' the value of that attribute is returned. Otherwise, it looks into the fields
#' and methods of the underlying GISDK object to determine what to do.
#'
#' @param x A \code{CaliperMatrix} object
#' @param name the method to dispatch
#' @export

`$.CaliperMatrix` <- function(x, name) {
  core_names <- names(.subset2(x, "cores"))
  # If the name references an R method/attribute
  if (exists(name, envir = x)) {
    .subset2(x, name)
  # If the name is a core
  } else if (name %in% core_names) {
    .subset2(x, "cores")[[name]]
  } else {
    stop(paste0(name, " is not a valid attribute or core name."))
  }
}
