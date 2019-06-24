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

#' Runs a GISDK macro
#'
#' A GISDK macro is a function defined in a GISDK scrip (.rsc). When working in
#' Caliper software, these are always called in GISDK using the
#' \code{RunMacro()} function.
#'
#' To run GISDK functions (like \code{OpenTable()}) see \code{\link{RunFunction}}.
#'
#' @param macro_name \code{string} Name of the GISDK macro to run
#' @param ... Used to pass arguments to the GISDK macro
#' @export
#' @examples
#' \dontrun{
#' # These won't work unless Caliper software is installed.
#' RunMacro("G30 Tutorial Folder")
#' RunMacro("add", 1, 2)
#' #> 3
#' RunMacro("parse opts array", list("one" = 1, "two" = 2))
#' #> "The first option name is one. The first option value is 1."
#' }

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
#' A GISDK function are core functions (like \code{OpenTable()}) that are called
#' in Caliper software without using \code{RunMacro()}.
#'
#' To run GISDK macros (like \code{RunMacro("G30 Tutorial Folder")}) see
#' \code{\link{RunMacro}}.
#'
#' @param macro_name \code{string} Name of the GISDK function to run
#' @param ... Used to pass arguments to the GISDK function
#' @export
#' @examples
#' \dontrun{
#' # These won't work unless Caliper software is installed.
#' table_name <- RunFunction("OpenTable", "airports", "ffb", list(paste0(folder, "airports.bin"), NA))
#' num_rows <- RunFunction("GetRecordCount", table_name, NA)
#' num_rows
#' #> 280
#' }

RunFunction <- function(macro_name,...) {
  dk <- get("CALIPER_DK", envir = caliper_env)
  gisdk_args <- process_gisdk_args(list(...))
  args <- c(list(macro_name), gisdk_args)
  result <- do.call(dk$RunMacro, args)
  return(result)
}

#' Change the Caliper UI
#'
#' Often, a user will have created their own GISDK functions and compiled them
#' to a UI file (.dbd). To run them (using \code{\link{RunMacro}}), first use
#' this function to point to the custom UI. This can also be used to set the
#' UI back to the default.
#'
#' To see the current UI, use \code{\link{GetInterface}}.
#'
#' @param ui_file \code{string} File path to the custom UI. If null, will set
#'   the interface back to the default.
#' @export

SetAlternateInterface <- function(ui_file = NULL) {
  if (is.null(ui_file) || ui_file == "default" || ui_file == "gis_ui") {
    ui_file = "gis_ui"
  } else {
    ui_file <- gsub("/", "\\", ui_file, fixed = TRUE)
    if (!file.exists(ui_file)){
      stop("caliperr::SetAlternateInterface: 'ui_file' not found")
    }
  }
  assign("CALIPER_UI", ui_file, envir = caliper_env)
}

#' Retrieves the current GISDK interface
#'
#' To set the current UI, use \code{\link{SetAlternateInterface}}.
#'
#' @export

GetInterface <- function() {
  ui_file <- get("CALIPER_UI", envir = caliper_env)
  if (ui_file == "gis_ui") {
    return("default")
  } else {
    ui_file <- gsub("\\", "/", ui_file, fixed = TRUE)
    return(ui_file)
  }
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
