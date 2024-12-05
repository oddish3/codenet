#' Parse Project Functions
#'
#' @param project_path Path to project directory
#' @param exclude_files Optional regex pattern for files to exclude
#' @return A list of functions with their details
#' @noRd
parse_project_functions <- function(project_path, exclude_files = NULL) {
  # Find all R files
  r_files <- list.files(
    path = project_path,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  )

  # Apply exclusions if any
  if (!is.null(exclude_files)) {
    r_files <- r_files[!grepl(exclude_files, r_files)]
  }

  # Create environment to store functions
  pkg_env <- new.env(parent = baseenv())

  # Source all files into the environment
  lapply(r_files, function(file) {
    try(source(file, local = pkg_env), silent = TRUE)
  })

  # Get all functions
  functions <- eapply(pkg_env, is.function)
  function_names <- names(functions)[unlist(functions)]

  return(list(
    names = function_names,
    env = pkg_env
  ))
}

#' Parse Memory Functions
#'
#' @param obj_names Character vector of object names
#' @param env Environment to look in
#' @return A list of functions with their details
#' @noRd
parse_memory_functions <- function(obj_names, env) {
  # Filter for function objects
  is_function <- sapply(obj_names, function(x) {
    is.function(get(x, envir = env))
  })
  function_names <- obj_names[is_function]

  # Create new environment with these functions
  pkg_env <- new.env(parent = baseenv())
  for(fname in function_names) {
    assign(fname, get(fname, envir = env), envir = pkg_env)
  }

  return(list(
    names = function_names,
    env = pkg_env
  ))
}


#' Parse Memory Objects
#'
#' @param obj_names Names of objects
#' @param object_types Types of objects to track
#' @param env Environment
#' @noRd
parse_memory_objects <- function(obj_names, object_types, env) {
  # Get object types
  obj_types <- sapply(obj_names, function(x) {
    obj <- get(x, envir = env)
    if (is.function(obj)) "function"
    else if (is.data.frame(obj)) "data.frame"
    else if (is.list(obj)) "list"
    else "other"
  })

  # Filter based on requested types
  keep <- switch(object_types,
    "functions" = obj_types == "function",
    "data.frames" = obj_types %in% c("data.frame", "list"),
    "all" = TRUE)

  filtered_names <- obj_names[keep]
  filtered_types <- obj_types[keep]

  # Create new environment with these objects
  pkg_env <- new.env(parent = baseenv())
  for (name in filtered_names) {
    assign(name, get(name, envir = env), envir = pkg_env)
  }

  return(list(
    names = filtered_names,
    types = filtered_types,
    env = pkg_env
  ))
}

#' Parse Project Objects
#'
#' @param project_path Path to project directory
#' @param object_types Types of objects to track
#' @return A list of objects with their details
#' @noRd
parse_project_objects <- function(project_path, object_types) {
  # Get all objects from project files
  pkg_env <- new.env(parent = baseenv())

  # Find and source all R files
  r_files <- list.files(
    path = project_path,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  )

  lapply(r_files, function(file) {
    try(source(file, local = pkg_env), silent = TRUE)
  })

  # Get all objects
  obj_list <- ls(pkg_env)

  # Get object types
  obj_types <- sapply(obj_list, function(x) {
    obj <- get(x, envir = pkg_env)
    if(is.function(obj)) "function"
    else if(is.data.frame(obj)) "data.frame"
    else if(is.list(obj)) "list"
    else "other"
  })

  # Filter based on requested types
  keep <- switch(object_types,
    "functions" = obj_types == "function",
    "data.frames" = obj_types == "data.frame",
    "all" = TRUE)

  filtered_names <- obj_list[keep]
  filtered_types <- obj_types[keep]

  return(list(
    names = filtered_names,
    types = filtered_types,
    env = pkg_env
  ))
}
