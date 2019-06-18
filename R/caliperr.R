#'Create a connection to a Caliper software product
#'
#'Will create a connection over COM to one of Caliper's installed software
#'products. (The software must be installed with a valid license.)
#'
#' @param software One of either "TransCAD", "TransModeler", or "Maptitude". If
#'  left \code{NULL}, the function will search (in that order) and create the first
#'  connection it can.
#' @import RDCOMClient
#' @return Nothing. Sets the COM object to a global environment variable
#'   (\code{caliper_dk})
#' @export

connect <- function(software = NULL){

  # Argument checking
  valid_software_values <- c("TransCAD", "TransModeler", "Maptitude")
  if (!is.null(software)){
    if (!(software %in% valid_software_values)) {
      stop(
        paste0(
          "(caliperr::connect) Invalid value for 'software'. Valid values are: ",
          paste(valid_software_values, collapse = ", ")
        )
      )
    }
  }

  # Try to connect if the user provided a value for `software`
  if (!is.null(software)) {
    tryCatch(
      dk <-  RDCOMClient::COMCreate(paste0(software, ".AutomationServer")),
      error = function(c) {
        c$message <- paste0(
          "Could not create a connection to ", software, ". ",
          "Check that ", software, " is installed."
        )
        stop(c)
      }
    )
  # If the user didn't provide a value for `software`
  } else {
    softwares <- valid_software_values[!is.null(valid_software_values)]
    for (software in softwares){
      suppressWarnings(
        try(
          dk <-  RDCOMClient::COMCreate(paste0(software, ".AutomationServer")),
          silent = TRUE
        )
      )
      if (exists("dk")) break
    }
  }

  if (!exists("dk")) stop(paste0(
    "Could not connect to any Caliper software. ",
    "Check that one of the following is installed: ",
    paste(softwares, collapse = ", ")
  ))

  assign("caliper_dk", dk, envir = .GlobalEnv)
  Sys.setenv(CALIPER_UI = "gis_ui")
}

#' Changes the default UI
#'
#' The default UI is simply "gis_ui", which Caliper software understands. If
#' several functions are going to be called from a custom UI compiled by the
#' user, this function can be used to change the default.
#'
#' @param ui \code{string} File name of the custom UI compiled by a user.

set_caliper_ui <- function(ui = "gis_ui") {

  # Argument checking
  if (ui != "gis_ui"){
    if (!file.exists(ui)) {
      stop("caliperr::set_caliper_ui: 'ui' file not found")
    }
  }

  Sys.setenv(CALIPER_UI = ui)
}

#' Runs a macro (function) in GISDK
#'
#' @param macro_name \code{string} Name of the GISDK macro to run
#' @param ui \code{string} Optional. Can be used to point to a custom UI
#'   compiled by the user. Defaults to \code{Sys.getenv("CALIPER_UI")}.
#' @param ... Used to pass arguments to the GISDK macro
#' @examples
#' \dontrun{
#' # These won't work unless Caliper software is installed.
#' run_macro("G30 Tutorial Folder")
#' run_macro("add", ui = ui_path, 1, 2)
#' # 3
#' run_macro("parse opts array", ui = ui_path, list("one" = 1, "two" = 2))
#' # "The first option name is one. The first option value is 1."
#' }

run_macro <- function(macro_name = NULL, ...) {

  # Check for COM connection to Caliper software
  obs <- objects(envir = .GlobalEnv)
  if (!("caliper_dk" %in% obs)) {
    caliperr::connect()
  }
  dk <- get("caliper_dk", envir=.GlobalEnv)

  # Argument checking
  if (is.null(macro_name)) stop(
    "caliperr::run_macro: 'macro_name' must be provided"
  )
  gisdk_args <- list(...)
  ui_passed_as_arg <- gisdk_args$ui
  gisdk_args$ui <- NULL
  if (is.null(ui_passed_as_arg)) {
    ui <- Sys.getenv("CALIPER_UI")
  } else {
    ui <- ui_passed_as_arg
  }
  if (ui != "gis_ui") {
    if (!file.exists(ui)) {
      stop("caliperr::run_macro: 'ui' file not found")
    }
  }
  gisdk_args <- process_gisdk_args(gisdk_args)

  # The following code attempts to work around a limitation in the
  # RDCOMClient (and SWinTypeLibs) libraries. In current versions of R,
  # they cannot inspect the methods of the COM class. In addition, if the
  # wrong method is called, the error message can be caught but not fully
  # suppressed.

  # In the following cases, RunUIMacro() should be used
  if (
    !is.null(ui_passed_as_arg) |
    ui != "gis_ui" |
    grepl(" ", macro_name)
  ) {
    try({
      args <- c(list(macro_name, ui), gisdk_args)
      result <- do.call(dk$RunUIMacro, args)
    }, silent = TRUE)
  }

  # If no output was created, attempt to call the GISDK macro through the
  # RunMacro interface.
  if (!exists("result")) {
    try({
      args <- c(list(macro_name), gisdk_args)
      result <- do.call(dk$RunMacro, args)
    }, silent = TRUE)
  }

  # Finally, use the RunUIMacro interface if neither of the above code
  # blocks produced output.
  if (!exists("result")) {
    try({
      args <- c(list(macro_name, ui), gisdk_args)
      result <- do.call(dk$RunUIMacro, args)
    }, silent = TRUE)
  }

  if (!exists("result")) stop("caliperr::run_macro failed")
  return(result)
}

#' Convert R arguments into GISDK flavors
#'
#' It calls \code{\link{create_named_array}} and
#' \code{\link{convert_to_gisdk_null}} as appropriate on each argument passed.
#'
#' @param arg_list \code{list} of args that are converted.
#' @keywords internal

process_gisdk_args <- function(arg_list) {

  if (length(arg_list) == 0) return(NULL)
  for (i in 1:length(arg_list)) {
    arg <- arg_list[[i]]
    if (!is.null(names(arg))) {
      arg <- create_named_array(arg)
      } else arg <- convert_to_gisdk_null(arg)
    arg_list[[i]] <- arg
  }
  return(arg_list)
}


#' Used internally to convert R's named vectors/lists to GISDK named arrays.
#'
#' @param named_list A named list (or vector)
#' @import RDCOMClient
#' @return a pointer object that GISDK will interpret as a named array
#' @keywords internal

create_named_array <- function(named_list) {

  # Argument checking
  if (is.null(names(named_list))) stop(
    "caliperr::create_opts_array: 'named_list' is not a named list"
  )

  df <- data.frame(
    names = names(named_list),
    values = unlist(unname(named_list))
  )
  df <- df[!is.null(df$values) & !is.na(df$values), ]

  return(RDCOMClient::asCOMArray(as.matrix(df)))
}

#' Converts R's \code{NA} and \code{NULL} to \code{NA_complex_)}
#'
#' \code{NA_complex_} is understood by GISDK/C++ as null.
#' @return Returns the argument passed in with any \code{NA/NULL} converted.
#' @keywords internal

convert_to_gisdk_null <- function(arg) {
  if (length(arg) == 1){
    if (is.null(arg) | is.na(arg)) {
      arg <- NA_complex_
    }
  }
  if (length(arg[is.na(arg) | is.null(arg)]) > 0){
    arg[is.na(arg) | is.null(arg)] <- NA_complex_
  }
  return(arg)
}
