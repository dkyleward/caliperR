#' Converts a Caliper view into an R data.frame
#'
#' @param view_name The name of the view open in Caliper software.
#' @param set_name An optional set name can be provided to only return
#'   records in a selection set from the view.
#' @keywords internal

view_to_df <- function(view_name, set_name = NULL) {
  # Make sure view_name is an open view
  software <- get_package_variable("CALIPER_SOFTWARE")
  tryCatch(
    {RunFunction("SetView", view_name)},
    error = function(e) {
      e$message <- paste0("View '", view_name, "' not open in ", software)
      stop(e)
    }
  )
  column_data <- RunFunction("GetFields", view_name, "All")
  column_names <- column_data[[1]]
  column_names <- gsub("\\[", "", column_names)
  column_names <- gsub("\\]", "", column_names)
  column_names <- as.list(column_names)
  viewset <- paste(view_name, set_name, sep = "|")
  data <- RunFunction(
    "GetDataVectors", viewset, column_names, list(OptArray = TRUE)
  )
  df <- as.data.frame(data)
}

#' Updates an existing Caliper view with data from a data.frame
#' @keywords internal

update_view <- function(df, view_name, set_name = NULL) {
  software <- get_package_variable("CALIPER_SOFTWARE")
  tryCatch(
    {RunFunction("SetView", view_name)},
    error = function(e) {
      e$message <- paste0("View '", view_name, "' not open in ", software)
      stop(e)
    }
  )

  RunMacro("update view from r", view_name, set_name, as.list(df))
}

#' Creates a view name guaranteed to be unique in the current Caliper session
#' @keywords internal

create_unique_view_name <- function() {
  current_views <- RunFunction("GetViews")
  for (i in 1:100) {
    view_name <- paste0("view", i)
    if (!(view_name %in% current_views)) return(view_name)
  }
}
