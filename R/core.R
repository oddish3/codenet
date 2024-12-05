#' Create Function Network
#'
#' Creates a network visualization of function dependencies from R files or R objects
#'
#' @param source Either a path to R files or a character vector of object names
#' @param type One of "files" or "objects" indicating the source type
#' @param env Environment to look for objects (default: parent.frame())
#' @return A visNetwork object representing function dependencies
#' @export
create_function_network <- function(source,
  type = c("files", "objects"),
  env = parent.frame()) {
  type <- match.arg(type)

  if (type == "files" && !dir.exists(source)) {
    stop("Directory ", source, " does not exist")
  }

  if (type == "objects") {
    missing_objects <- source[!sapply(source, exists, envir = env)]
    if (length(missing_objects) > 0) {
      stop("Objects not found: ", paste(missing_objects, collapse = ", "))
    }
  }

  # Get functions based on source type
  functions <- if(type == "files") {
    parse_project_functions(source)
  } else {
    parse_memory_functions(source, env)
  }

  if (length(functions$names) == 0) {
    warning("No functions found in the specified source")
  }

  edges <- identify_function_dependencies(functions)
  network <- plot_network(edges)
  return(network)
}

#' Create Object Network
#'
#' @param source Source of objects (files or environment)
#' @param type Type of analysis ("files", "objects")
#' @param object_types Types of objects to track ("functions", "data.frames", "all")
#' @param env Environment to look in
#' @export
create_object_network <- function(source,
  type = c("files", "objects"),
  object_types = c("functions", "data.frames", "all"),
  env = parent.frame()) {
  type <- match.arg(type)
  object_types <- match.arg(object_types)

  # Check if all objects exist
  if (type == "objects") {
    missing_objects <- source[!sapply(source, exists, envir = env)]
    if (length(missing_objects) > 0) {
      stop("The following objects were not found: ",
        paste(missing_objects, collapse = ", "))
    }
  }

  objects <- if(type == "files") {
    parse_project_objects(source, object_types)
  } else {
    parse_memory_objects(source, object_types, env)
  }

  edges <- identify_dependencies(objects)
  network <- plot_enhanced_network(edges, objects$types)
  return(network)
}

