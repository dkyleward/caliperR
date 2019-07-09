#' caliper: Communicate with Caliper software from R
#'
#' The caliper package provides functions to make it easier to communicate
#' with Caliper software over COM.
#'
#' @docType package
#' @name caliper
NULL

#'Create a connection to a Caliper software product
#'
#'Will create a connection over COM to one of Caliper's installed software
#'products. (The software must be installed with a valid license.)
#'
#' @param software One of either "TransCAD", "TransModeler", or "Maptitude". If
#'  left \code{NULL}, the function will search (in that order) and create the first
#'  connection it can.
#' @param silent \code{boolean} Whether to display a connected message.
#' @import RDCOMClient
#' @return Nothing. Sets the COM object to a global environment variable
#'   (\code{CALIPER_DK})
#' @export

connect <- function(software = NULL, silent = FALSE){

  # Argument checking
  valid_software_values <- c("TransCAD", "TransModeler", "Maptitude")
  if (!is.null(software)){
    if (!(software %in% valid_software_values)) {
      stop(
        paste0(
          "(caliper::connect) Invalid value for 'software'. Valid values are: ",
          paste(valid_software_values, collapse = ", ")
        )
      )
    }
  }
  if (!is.logical(silent)) {
    stop("(caliper::connect) 'silent' must be logical (true/false)")
  }

  # To prevent orphan processes, disconnect previous connections if the user
  # connects to a different software. e.g. was connected to TransCAD and now
  # wants to connect to Maptitude.
  if (connected() && !is.null(software)) {
    current_software <- get_package_variable("CALIPER_SOFTWARE")
    if (software != current_software) disconnect()
  }

  if (is.null(software))
    software_to_try <- valid_software_values
  else
    software_to_try <- software
  for (software in software_to_try) {
    suppressWarnings(
      try(
        {
          dk <-  RDCOMClient::COMCreate(paste0(software, ".AutomationServer"))
          set_package_variable("CALIPER_DK", dk)
          set_package_variable("CALIPER_SOFTWARE", software)
          set_package_variable("CALIPER_UI", "gis_ui")
        },
        silent = TRUE
      )
    )
    if (exists("dk")) break
  }

  if (!exists("dk")) stop(
    "Could not connect to Caliper software. Check that it is installed."
  )
  if (!silent) {
    message("Connected to ", software)
  }
}

#' Close the COM connection to Caliper software and kills the process
#'
#' @return Nothing.
#' @export

disconnect <- function() {
  if (connected()) {
    RunFunction("Exit")
    remove("CALIPER_DK", envir = caliper_env)
    remove("CALIPER_SOFTWARE", envir = caliper_env)
    remove("CALIPER_UI", envir = caliper_env)
  }
}

#' Checks if R is connected to Caliper software
#'
#' @export

connected <- function() {
  try(
    dk <- get_package_variable("CALIPER_DK"),
    silent = TRUE
  )
  if (exists("dk"))
    return(TRUE)
  else
    return(FALSE)
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

RunMacro <- function(macro_name, ...) {
  stopifnot(connected())
  dk <- get_package_variable("CALIPER_DK")
  dk_ui <- get_package_variable("CALIPER_UI")
  gisdk_args <- process_gisdk_args(list(...))
  args <- c(list(macro_name, dk_ui), gisdk_args)
  result <- do.call(dk$RunUIMacro, args)
  result <- process_gisdk_result(result)
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
  stopifnot(connected())
  invalid_macro_names <- c(
    "RunMacro",
    "CreateObject",
    "SetAlternateInterface",
    "GetInterface"
  )
  if (macro_name %in% invalid_macro_names) {
    stop(paste0(
      "Use caliper::", macro_name, "()"
    ))
  }
  dk <- get_package_variable("CALIPER_DK")
  gisdk_args <- process_gisdk_args(list(...))
  args <- c(list(macro_name), gisdk_args)
  result <- do.call(dk$RunMacro, args)
  result <- process_gisdk_result(result)
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
      stop("(caliper::SetAlternateInterface) 'ui_file' not found")
    }
  }
  set_package_variable("CALIPER_UI", ui_file)
}

#' Retrieves the current GISDK interface
#'
#' To set the current UI, use \code{\link{SetAlternateInterface}}.
#'
#' @export

GetInterface <- function() {
  ui_file <- get_package_variable("CALIPER_UI")
  if (ui_file == "gis_ui") {
    return("default")
  } else {
    ui_file <- gsub("\\", "/", ui_file, fixed = TRUE)
    return(ui_file)
  }
}

#' Convert R arguments into GISDK flavors
#'
#' It calls \code{\link{convert_to_named_array}} and
#' \code{\link{convert_nulls_and_slashes}} as appropriate on each argument passed.
#'
#' @param arg_list \code{list} of args that are converted.
#' @keywords internal

process_gisdk_args <- function(arg_list) {

  if (length(arg_list) == 0) return(NULL)
  for (i in 1:length(arg_list)) {
    arg <- arg_list[[i]]
    if (is.object(arg)) next
    if (!is.null(names(arg))) {
      arg <- convert_to_named_array(arg)
    } else arg <- convert_nulls_and_slashes(arg)
    if (length(arg) == 0) arg <- NULL
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

convert_to_named_array <- function(named_list) {

  # Argument checking
  if (is.null(names(named_list))) stop(
    "(caliper::create_opts_array) 'named_list' is not a named list"
  )

  df <- data.frame(
    names = names(named_list),
    values = unlist(unname(named_list))
  )
  df <- df[!is.null(df$values) & !is.na(df$values), ]

  return(RDCOMClient::asCOMArray(as.matrix(df)))
}

#' Used to convert GISDK named arrays to R's named lists.
#'
#' When a GISDK named array comes across COM, it has a specific format that
#' isn't easy to use. This converts it into an R named list.
#'
#' @param nested_list The list object that results whenever GISDKs named arrays
#'   are passed through COM.
#' @return A named list
#' @keywords internal

convert_to_named_list <- function(nested_list) {
  names <- unlist(lapply(nested_list, "[", 1))
  values <- unlist(lapply(nested_list, "[", 2), recursive = FALSE)
  names(values) <- names
  return(values)
}

#' Checks if an object is a GISDK named array
#'
#' When returned over COM, these arrays have a very specific format. This
#' function is used in conjunction with \code{\link{convert_to_named_list}} in
#' \code{\link{process_gisdk_result}} to identify and convert GISDK named arrays
#' into R's named lists.
#'
#' @param object The object to be tested
#' @return \code{boolean} TRUE/FALSE
#' @keywords internal

is_gisdk_named_array <- function(object) {
  if (typeof(object) != "list") return(FALSE)
  v1 <- lapply(object, typeof) == "list"
  v2 <- lapply(object, length) == 2
  v3 <- lapply(object, function(x) typeof(x[[1]])) == "character"
  if (all(c(v1, v2, v3))) return(TRUE) else return(FALSE)
}

#' Converts R's \code{NA}, \code{NULL}, and \code{/} to formats GISDK can use
#'
#' \code{NA} and \code{NULL} are converted to \code{NA_complex_} and is
#' understood by GISDK/C++ as \code{null}. \code{/} is converted to \code{\\}.
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
  if (caliper:::is_gisdk_named_array(result)) {
    return(caliper:::convert_to_named_list(result))
  }
  if (class(result) != "COMIDispatch") return(result)
  type <- RunMacro("get_object_type", result)
  if (type == "vector") result <- RunFunction("V2A", result)
  if (type == "matrix") result <- CaliperMatrix$new(result)

  return(result)
}


#' Sets the value of a package-wide variable
#'
#' \code{caliper} uses several package-wide variables to enable communication
#' between functions and simplify function arguments.
#'
#' @param package_variable The package variable to set
#' @param value the value to set with
#' @keywords internal

set_package_variable <- function(package_variable, value) {

  package_variables <- c("CALIPER_DK", "CALIPER_SOFTWARE", "CALIPER_UI")
  if (!(package_variable %in% package_variables)) {
    stop(paste(
      "'package_variable' must be one of",
      paste(package_variables, collapse = ", ")
    ))
  }

  if (package_variable == "CALIPER_DK" & class(value) != "COMIDispatch") {
    stop("CALIPER_DK must be class COMIDispatch")
  }

  software_options <- c("TransCAD", "TransModeler", "Maptitude")
  if (package_variable == "CALIPER_SOFTWARE" && !(value %in% software_options)) {
    stop(paste(
      "CALIPER_SOFTWARE must be one of",
      paste(software_options, collapse = ", ")
    ))
  }

  if (package_variable == "CALIPER_UI") {
    if (typeof(value) != "character") stop("'value' must be a file path")
    if (!file.exists(value) & value != "gis_ui") {
      stop("CALIPER_UI file does not exist")
    }
  }

  assign(package_variable, value, envir = caliper_env)
}

#' Gets the value of a \code{caliper} package-wide variable
#'
#' @inheritParams set_package_variable
#' @export

get_package_variable <- function(package_variable) {
  return(get(package_variable, envir = caliper_env))
}
