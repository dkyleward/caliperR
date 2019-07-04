#' S3 method for converting a \code{matrix_handle} into a \code{data.frame}
#'
#' @param x \code{matrix_handle}
#' @param row.names See \code{as.data.frame}
#' @param optional See \code{as.data.frame}
#' @param ... additional arguments passed to \code{as.data.frame}
#' @import data.table
#' @export

as.data.frame.matrix_handle <- function(x, row.names = NULL,
                                        optional = FALSE, ...) {

  temp_file <- tempfile(fileext = ".csv")
  core_names <- names(x$cores)
  RunFunction(
    "CreateTableFromMatrix", x$ref, temp_file, "CSV",
    list(Complete = TRUE)
  )
  df <- data.table::fread(temp_file, header = FALSE)
  colnames(df) <- c("from", "to", core_names)
  setDF(df, rownames = row.names)
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
#' @param x A \code{matrix_currency} object
#' @param ... Additional arguments passed to \code{as.matrix}. An extra argument
#'   \code{core} can be used to specify which core to convert. See details
#' @export

as.matrix.matrix_handle <- function(x, ...) {

  # Argument checking
  args <- list(...)
  if (!is.null(args$core)) {
    core <- args$core
    stopifnot(core %in% names(x$cores))
  } else {
    core <- names(x$cores)[1]
  }

  tbl <- as.data.frame.matrix_handle(x)

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

#' S3 method for summarizing a \code{matrix_handle}
#'
#' @param x \code{matrix_handle}
#' @param ... Additional arguments (not used)
#' @import data.table
#' @export

summary.matrix_handle <- function(x, ...) {

  # Argument checking
  stopifnot(class(x) == "matrix_handle")

  stats <- RunFunction("MatrixStatistics", x$ref, NA)
  list_of_rows <- lapply(stats, function(x) {
    core_name <- x[[1]]
    stat_names <- unlist(lapply(x[[2]], function(x) {
      stat_name <- x[[1]]
    }))
    stat_values <- unlist(lapply(x[[2]], function(x) {
      stat_value <- x[[2]]
    }))
    df <- as.data.frame(stat_values)
    df <- data.table::transpose(df)
    colnames(df) <- stat_names
    df$Core <- core_name
    return(df)
  })

  df <- data.table::rbindlist(list_of_rows)
  setcolorder(df, "Core")
  return(df)
}

#' Creates an S3 object of class \code{matrix_handle}
#'
#' A \code{matrix_handle} is different from an R matrix. This object represents
#' all the cores of a Caliper matrix.
#'
#' @param m Either a file name to read (.mtx) or a COM pointer to an open matrix
#'   in Caliper software.
#' @export

create_matrix <- function(m) {
  if(inherits(m, "matrix_handle")) return(m)
  if (!(class(m) %in% c("character", "COMIDispatch")))
    stop("(caliper::create_matrix) 'm' must be either a character or COM pointer")
  if (typeof(m) == "character"){
    if (!file.exists(m))
      stop("(caliper::create_matrix) file 'm' not found")
    mh <- RunFunction("OpenMatrix", m, NA)
  } else {
    mh <- m
  }

  # Calling RunFunction() above will call process_gisdk_results(), which can
  # call create_matrix() again. The if statement determines if it's the first
  # or second call.
  if (class(mh) == "COMIDispatch") {
    core_names <- RunFunction("GetMatrixCoreNames", mh)
    cores <- create_matrix_cores(mh)
    obj <- structure(
      list(ref = mh, cores = cores),
      class = "matrix_handle"
    )
    return(obj)
  }
  if(inherits(mh, "matrix_handle")) return(mh)
}

#' Internal function used to create currency pointers for all cores in a matrix
#'
#' This is the function that creates the list of COM pointers (one for each core)
#' that is stored in a \code{matrix_handle} object.
#' @param mh \code{COMIDispatch} A pointer to the GISDK matrix handle
#' @param ri \code{string} Name of the row index. (NULL for base index)
#' @param ci \code{string} Name of the column index. (NULL for base index)
#' @keywords internal

create_matrix_cores <- function(mh, ri = NULL, ci = NULL) {
  if (class(mh) != "COMIDispatch")
    stop("(caliper::create_matrix_cores) 'mh' isn't the right class")
  if (is.null(ri)) ri <- NA
  if (is.null(ci)) ci <- NA

  currencies <- RunFunction("CreateMatrixCurrencies", mh, ri, ci, NA)
  cores <- list()
  for (i in 1:length(currencies)) {
    name <- currencies[[i]][[1]]
    pointer <- currencies[[i]][[2]]

    # cur <- structure(
    #   list(pointer),
    #   class = "matrix_currency"
    # )

    cores[[name]] <- pointer
  }
  return(cores)
}
