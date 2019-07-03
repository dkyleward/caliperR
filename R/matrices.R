#' S3 generic for printing a \code{matrix_handle}
#'
#' @param x A \code{matrix_handle} object
#' @param ... Additional options
#' @export

print.matrix_handle_temp <- function(x, ...) {
  # place holder. Change to print.matrix_handle and define behavior.
}

#' S3 generic for printing a matrix currency
#'
#' @param x A \code{matrix_currency} object
#' @param ... Additional options
#' @export

print.matrix_currency_temp <- function(x, ...) {
  temp_file <- tempfile(fileext = ".omx")
  com_obj <- RunFunction(
    "CopyMatrix", x[[1]],
    list(
      "File Name" = temp_file,
      "Label" = "test",
      "OMX" = "true"
    )
  )
}

#' Creates an S3 object of class \code{matrix_handle}
#'
#' A \code{matrix_handle} is different from an R matrix. This object represents
#' all the cores of a Caliper matrix.
#'
#' @param file File name to read (.mtx)
#' @export

create_matrix <- function(file) {
  if (!file.exists(file))
    stop("(caliper::create_matrix_currency) 'file' not found")
  mh <- RunFunction("OpenMatrix", file, NA)
  core_names <- RunFunction("GetMatrixCoreNames", mh)
  currencies <- create_matrix_currencies(mh)
  result <- structure(
    list(ref = mh, currencies = currencies),
    class = "matrix_handle"
  )
}

#' Internal function used to create currency pointers for all cores in a matrix
#'
#' This is the function that creates the list of COM pointers (one for each core)
#' that is stored in a \code{matrix_handle} object.
#' @param mh \code{COMIDispatch} A pointer to the GISDK matrix handle
#' @param ri \code{string} Name of the row index. (NULL for base index)
#' @param ci \code{string} Name of the column index. (NULL for base index)
#' @keywords internal

create_matrix_currencies <- function(mh, ri = NULL, ci = NULL) {
  if (class(mh) != "COMIDispatch")
    stop("(caliper::create_matrix_currencies) 'mh' isn't the right class")
  if (is.null(ri)) ri <- NA
  if (is.null(ci)) ci <- NA

  currencies <- RunFunction("CreateMatrixCurrencies", mh, ri, ci, NA)
  result <- list()
  for (i in 1:length(currencies)) {
    name <- currencies[[i]][[1]]
    pointer <- currencies[[i]][[2]]

    cur <- structure(
      list(pointer),
      class = "matrix_currency"
    )

    result[[name]] <- cur
  }
  return(result)
}
