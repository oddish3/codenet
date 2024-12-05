#' Identify Function Dependencies
#'
#' @param functions List containing function names and their environment
#' @return A data frame of edges representing function dependencies
#' @noRd
identify_function_dependencies <- function(functions) {
  edges <- data.frame(from = character(), to = character())

  for (func_name in functions$names) {
    func <- get(func_name, envir = functions$env)
    func_calls <- find_function_calls(func, functions$names)

    if (length(func_calls) > 0) {
      new_edges <- data.frame(
        from = rep(func_name, length(func_calls)),
        to = func_calls
      )
      edges <- rbind(edges, new_edges)
    }
  }

  return(unique(edges))
}

#' Identify Dependencies
#'
#' @param objects List containing object names, types, and their environment
#' @return A data frame of edges representing dependencies
#' @noRd
identify_dependencies <- function(objects) {
  edges <- data.frame(from = character(), to = character())

  for (obj_name in objects$names) {
    obj <- get(obj_name, envir = objects$env)

    if (is.function(obj)) {
      # For functions, look for function calls and data usage
      deps <- find_function_dependencies(obj, objects)
      if (length(deps) > 0) {
        new_edges <- data.frame(
          from = rep(obj_name, length(deps)),
          to = deps
        )
        edges <- rbind(edges, new_edges)
      }
    }
  }

  return(unique(edges))
}

#' Find Function Dependencies
#'
#' @param func Function to analyze
#' @param objects List of available objects
#' @return Character vector of dependency names
#' @noRd
find_function_dependencies <- function(func, objects) {
  # Get function body as character
  body_chr <- paste(deparse(body(func)), collapse = "\n")

  # Find both function calls and data frame usage
  deps <- character()

  # Look for function calls
  for (name in objects$names[objects$types == "function"]) {
    if (grepl(paste0("\\b", name, "\\b"), body_chr)) {
      deps <- c(deps, name)
    }
  }

  # Look for data frame usage
  for (name in objects$names[objects$types == "data.frame"]) {
    if (grepl(paste0("\\b", name, "\\b"), body_chr)) {
      deps <- c(deps, name)
    }
  }

  return(unique(deps))
}

#' Find Data Frame Dependencies
#'
#' @param df_name Name of the data frame
#' @param objects List of available objects
#' @return Character vector of function names that use this data frame
#' @noRd
find_dataframe_dependencies <- function(df_name, objects) {
  deps <- character()

  # Look for data frame usage in functions
  for (func_name in objects$names[objects$types == "function"]) {
    func <- get(func_name, envir = objects$env)
    body_chr <- paste(deparse(body(func)), collapse = "\n")

    if (grepl(paste0("\\b", df_name, "\\b"), body_chr)) {
      deps <- c(deps, func_name)
    }
  }

  return(unique(deps))
}
