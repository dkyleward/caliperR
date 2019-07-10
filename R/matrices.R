#' S3 method for converting a \code{CaliperMatrix} into a \code{data.frame}
#'
#' @param x \code{CaliperMatrix}
#' @param row.names See \code{as.data.frame}
#' @param optional See \code{as.data.frame}
#' @param ... additional arguments passed to \code{as.data.frame}
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
#' By default, the first core of the Caliper matrix is converted into an R matrix.
#' To select a different one, use the \code{core} argument like so:
#' \code{as.matrix(x, core = "second core")}.
#'
#' @param x A \code{CaliperMatrix} object
#' @param ... Additional arguments passed to \code{as.matrix}. An extra argument
#'   \code{core} can be used to specify which core to convert. See details
#' @export

as.matrix.CaliperMatrix <- function(x, ...) {

  # Argument checking
  args <- list(...)
  if (!is.null(args$core)) {
    core <- args$core
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
#' @param x \code{CaliperMatrix}
#' @param ... Additional arguments (not used)
#' @import data.table
#' @export

summary.CaliperMatrix <- function(object, ...) {

  # Argument checking
  stopifnot("CaliperMatrix" %in% class(object))

  stats <- RunFunction("MatrixStatistics", object$handle, NA)
  list_of_rows <- Map(function(x, name) {
    stat_names <- unlist(lapply(x, "[[", 1))
    stat_values <- unlist(lapply(x, "[[", 2))
    df <- as.data.frame(stat_values)
    df <- data.table::transpose(df)
    colnames(df) <- stat_names
    df$Core <- name
    return(df)
  }, stats, names(stats))

  df <- data.table::rbindlist(list_of_rows)
  setcolorder(df, "Core")
  return(df)
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
      self$indices <- RunFunction("GetMatrixIndexNames", self$handle)
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
      if (!(name %in% self$indices)) {
        stop(
          paste0("Name must be one of ", paste(self$indices, collapse = ", "))
        )
      }
      self$cores <- NULL
      private$current_row_index <- name
      self$create_matrix_cores()
    },
    column_index = function (name) {
      if (missing(name)) return(private$current_column_index)
      if (!(name %in% self$indices)) {
        stop(
          paste0("Name must be one of ", paste(self$indices, collapse = ", "))
        )
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
