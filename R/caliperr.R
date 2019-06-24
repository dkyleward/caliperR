#' caliperr: Communicate with Caliper software from R
#'
#' The caliperr package provides functions to make it easier to communicate
#' with Caliper software over COM.
#'
#'
#' @docType package
#' @name caliperr
NULL

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
#'   (\code{CALIPER_DK})
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
      {
        dk <-  RDCOMClient::COMCreate(paste0(software, ".AutomationServer"))
        assign("CALIPER_SOFTWARE", software, envir = caliper_env)
      },
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
    for (software in valid_software_values){
      suppressWarnings(
        try(
          {
            dk <-  RDCOMClient::COMCreate(paste0(software, ".AutomationServer"))
            assign("CALIPER_SOFTWARE", software, envir = caliper_env)
          },
          silent = TRUE
        )
      )
      if (exists("dk")) break
    }
  }

  if (!exists("dk")) stop(paste0(
    "Could not connect to any Caliper software. ",
    "Check that one of the following is installed: ",
    paste(valid_software_values, collapse = ", ")
  ))

  assign("CALIPER_DK", dk, envir = caliper_env)
  assign("CALIPER_UI", "gis_ui", envir = caliper_env)
}

#' Close the COM connection to Caliper software and kills the process
#'
#' @return Nothing.
#' @export

disconnect <- function() {

  try(
    software <- get("CALIPER_SOFTWARE", envir = caliper_env),
    silent = TRUE
  )
  if (exists("software", inherits = FALSE)) {
    process_names <- list(
      "TransCAD" = "tcw",
      "TransModeler" = "tsm",
      "Maptitude" = "mapt"
    )
    process <- process_names[[software]]
    system(paste0("tskill ", process))
  }
}

#' Changes the default UI
#'
#' The default UI is "gis_ui", which Caliper software understands. If several
#' functions are going to be called from a custom GISDK UI compiled by the user,
#' this function can be used to change the default. Doing so is merely
#' convenience and avoids having to pass the custom UI path into each call to
#' \code{run_macro()}.
#'
#' @param ui \code{string} File name of the custom UI compiled by a user.
#' @export

set_caliper_ui <- function(ui = "gis_ui") {

  # Argument checking
  if (ui != "gis_ui"){
    if (!file.exists(ui)) {
      stop("caliperr::set_caliper_ui: 'ui' file not found")
    }
  }

  assign("CALIPER_UI", ui, envir = caliper_env)
}

#' Runs a macro (function) in GISDK
#'
#' @param macro_name \code{string} Name of the GISDK macro to run
#' @param ui \code{string} Optional. Can be used to point to a custom UI
#'   compiled by the user. Defaults to \code{Sys.getenv("CALIPER_UI")}.
#' @param ... Used to pass arguments to the GISDK macro
#' @export
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
  obs <- objects(envir = caliper_env)
  if (!("CALIPER_DK" %in% obs)) {
    caliperr::connect()
  }
  dk <- get("CALIPER_DK", envir=caliper_env)

  # Argument checking
  if (is.null(macro_name)) stop(
    "caliperr::run_macro: 'macro_name' must be provided"
  )
  gisdk_args <- list(...)
  ui_passed_as_arg <- gisdk_args$ui
  gisdk_args$ui <- NULL
  if (is.null(ui_passed_as_arg)) {
    ui <- get("CALIPER_UI", envir=caliper_env)
  } else {
    ui <- ui_passed_as_arg
  }
  ui <- gsub("/", "\\", ui, fixed = TRUE)
  if (!(ui == "gis_ui" | file.exists(ui))){
    stop("caliperr::run_macro: 'ui' file not found")
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
  if (!exists("result", inherits = FALSE)) {
    try({
      args <- c(list(macro_name), gisdk_args)
      result <- do.call(dk$RunMacro, args)
    }, silent = TRUE)
  }

  # Finally, use the RunUIMacro interface if neither of the above code
  # blocks produced output.
  if (!exists("result", inherits = FALSE)) {
    try({
      args <- c(list(macro_name, ui), gisdk_args)
      result <- do.call(dk$RunUIMacro, args)
    }, silent = TRUE)
  }

  if (!exists("result", inherits = FALSE)) stop("caliperr::run_macro failed")
  result <- process_gisdk_result(result)
  return(result)
}

#' #' Runs a macro in GISDK
#'
#' @export

RunMacro <- function(macro_name,...) {
  dk <- get("CALIPER_DK", envir = caliper_env)
  dk_ui <- get("CALIPER_UI", envir = caliper_env)
  if (is.null(dk_ui)) {
    dk_ui = "gis_ui"
  }
  gisdk_args <- process_gisdk_args(list(...))
  args <- c(list(macro_name, dk_ui), gisdk_args)
  result <- do.call(dk$RunUIMacro, args)
  return(result)
}

#' Runs a GISDK function
#'
#' @export

RunFunction <- function(macro_name,...) {
  dk <- get("CALIPER_DK", envir = caliper_env)
  gisdk_args <- process_gisdk_args(list(...))
  args <- c(list(macro_name), gisdk_args)
  result <- do.call(dk$RunMacro, args)
  return(result)
}

#' Change the Caliper UI
#'
#' @export

SetAlternateInterface <- function(ui_file) {
  if (is.null(ui_file)) {
    ui_file = "gis_ui"
  } else {
    ui_file <- gsub("/", "\\", ui_file, fixed = TRUE)
    if (!file.exists(ui_file)){
      stop("caliperr::SetAlternateInterface: 'ui_file' not found")
    }
  }
  assign("CALIPER_UI", ui_file, envir = caliper_env)
}



#' Convert R arguments into GISDK flavors
#'
#' It calls \code{\link{create_named_array}} and
#' \code{\link{convert_nulls_and_slashes}} as appropriate on each argument passed.
#'
#' @param arg_list \code{list} of args that are converted.
#' @keywords internal

process_gisdk_args <- function(arg_list) {

  if (length(arg_list) == 0) return(NULL)
  for (i in 1:length(arg_list)) {
    arg <- arg_list[[i]]
    if (class(arg) == "COMIDispatch") next
    if (!is.null(names(arg))) {
      arg <- create_named_array(arg)
      } else arg <- convert_nulls_and_slashes(arg)
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

#' Converts R's \code{NA}, \code{NULL}, and \code{/} formats GISDK can use
#'
#' \code{NA} and \code{NULL} are converted to \code{NA_complex_} and is
#' understood by GISDK/C++ as null. \code{/} is converted to \code{\\}.
#' @return Returns the argument passed in with any \code{NA/NULL} converted.
#' @keywords internal

convert_nulls_and_slashes <- function(arg) {
  if (length(arg) == 1){
    if (is.null(arg) | is.na(arg)) {
      arg <- NA_complex_
    }
    if (is.character(arg)) arg <- gsub("/", "\\", arg, fixed = TRUE)
  }
  if (length(arg[is.na(arg) | is.null(arg)]) > 0){
    arg[is.na(arg) | is.null(arg)] <- NA_complex_
  }
  if (length(arg[unlist(lapply(arg, is.character))]) > 0){
    arg[grep("/", unlist(arg))] <- gsub("/", "\\", arg[grep("/", unlist(arg))], fixed = TRUE)
  }
  return(arg)
}

#' Converts GISDK output into R structures
#'
#' Simple objects pass through the COM interface as usable R structures.
#' GISDK arrays, for example, become R vectors. Other things, like GISDK
#' vectors, come across as pointers to the object inside the Caliper
#' process. This function attemps to coerce those pointers into usable
#' R data structures.
#'
#' @param result A returned value from Caliper software.
#' @keywords internal

process_gisdk_result <- function(result) {
  if (class(result) != "COMIDispatch") return(result)
  try({
    result <- run_macro("V2A", result)
    return(result)
  }, silent = TRUE)

}
