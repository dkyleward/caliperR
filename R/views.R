#' Converts a Caliper view into an R data.frame
#'
#' @param view_name The name of the view open in Caliper software.
#' @param set_name An optional set name can be provided to only return
#'   records in a selection set from the view.
#' @export

view_to_df <- function(view_name, set_name = NULL) {
  # Make sure view_name is an open view
  current_views <- RunFunction("GetViews")[[1]]
  if (!(view_name %in% current_views)){
    software <- get_package_variable("CALIPER_SOFTWARE")
    stop("View '", view_name, "' not open in ", software)
  }
  if (!is.null(set_name)) {
    current_sets <- unlist(RunFunction("GetSets", view_name))
    if (!(set_name %in% current_sets)){
      stop("Set '", set_name, "' not in view '", view_name, "'")
    }
  }
  column_data <- RunFunction("GetFields", view_name, "All")
  column_names <- column_data[[1]]
  column_names <- gsub("\\[", "", column_names)
  column_names <- gsub("\\]", "", column_names)
  column_names <- as.list(column_names)
  viewset <- paste(view_name, set_name, sep = "|")
  data <- RunFunction(
    "GetDataVectors", viewset, column_names, list(OptArray = TRUE)
  )
  df <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  return(df)
}

#' Converts a data.frame into a view. Updates an existing view or creates
#' a new one as needed.
#' @export

df_to_view <- function(df, view_name = NULL, set_name = NA_complex_) {

  if (!is.null(view_name)) update_view(df, view_name, set_name)
  else {
    view_name <- create_view(df)
    return(view_name)
  }
}

#' Updates an existing Caliper view with data from a data.frame
#' @keywords internal

update_view <- function(df, view_name, set_name = NA_complex_) {
  # Make sure view_name and set_name exist
  current_views <- unlist(RunFunction("GetViews"))
  if (!(view_name %in% current_views)) {
    software <- get_package_variable("CALIPER_SOFTWARE")
    stop("View '", view_name, "' not open in ", software)
  }
  if (!is.na(set_name)) {
    current_sets <- unlist(RunFunction("GetSets", view_name))
    if (!(set_name %in% current_sets)){
      stop("Set '", set_name, "' not in view '", view_name, "'")
    }
  }

  SetAlternateInterface(get_package_variable("GISDK_UTILS_UI"))
  gplyr <- CreateObject("gplyr", as.list(df))
  gplyr$update_view(view_name, set_name)
  SetAlternateInterface()
  return(view_name)
}

#' Creates a new Caliper view with data from a data.frame
#' @keywords internal

create_view <- function(df) {

  SetAlternateInterface(get_package_variable("GISDK_UTILS_UI"))
  gplyr <- CreateObject("gplyr", as.list(df))
  view_name <- create_unique_view_name()
  gplyr$create_view()
  SetAlternateInterface()
  return(view_name)
}

#' Creates a view name guaranteed to be unique in the current Caliper session
#' @keywords internal

create_unique_view_name <- function() {
  current_views <- RunFunction("GetViews")
  for (i in 1:100) {
    view_name <- paste0("gplyr", i)
    if (!(view_name %in% current_views)) return(view_name)
  }
}
