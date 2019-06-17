#'Create a connection to a Caliper software product
#'
#'Will create a connection over COM to one of Caliper's installed software
#'products. (The software must be installed with a valid license.)
#'
#'@param software One of either "TransCAD", "TransModeler", or "Maptitude". If
#'  left \code{NULL}, the function will search (in that order) and create the first
#'  connection it can.
#'@import RDCOMClient
#'@return Nothing. Sets the COM object to a global environment variable
#'  (\code{caliper_dk})
#'@export

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

  assign("dk", dk, envir = .GlobalEnv)
}

#' Used internally to convert R's named lists to GISDK named arrays.
#'
#' @param named_list
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
    values = unname(named_list)
  )

  RDCOMClient::asCOMArray(as.matrix(df))
}
