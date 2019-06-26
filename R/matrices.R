# for testing
# folder <- RunMacro("G30 Tutorial Folder")
# file <- paste0(folder, "Accessibility Skim BusWlk.mtx")
# matrix <- create_matrix(file)
# x <- matrix$currencies$`In-Vehicle Time`

print <- function (x, ...) {
  UseMethod("print", x)
}

print.matrix_handle <- function(x, ...) {

}

print.matrix_currency <- function(x, ...) {
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

create_matrix <- function(file) {
  if (!file.exists(file))
    stop("(caliper::create_matrix_currency) 'file' not found")
  mh <- RunFunction("OpenMatrix", file, NA)
  core_names <- RunFunction("GetMatrixCoreNames", mh)
  currencies <- create_matrix_currencies(mh)
  result <- structure(
    list(handle = mh, currencies = currencies),
    class = "matrix_handle"
  )
}

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
