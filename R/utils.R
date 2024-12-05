#' Find Function Calls
#'
#' Extracts function calls from a function body
#'
#' @param func Function to analyze
#' @param available_funcs Vector of available function names
#' @return Character vector of called function names
#' @noRd
find_function_calls <- function(func, available_funcs) {
  # Get function body as character and collapse into single string
  body_chr <- paste(deparse(body(func)), collapse = "\n")

  # Find matches with available functions
  calls <- character()
  for (f in available_funcs) {
    # Skip if function would match itself
    if (identical(f, deparse(substitute(func)))) next

    # Look for function name with word boundaries
    if (grepl(paste0("\\b", f, "\\b"), body_chr)) {
      calls <- c(calls, f)
    }
  }

  return(unique(calls))
}


#' Parse Function Body
#'
#' Helper function to parse function body and extract components
#'
#' @param x Expression to parse
#' @return Character vector of parsed components
#' @noRd
parse_function_body <- function(x) {
  if (!is.atomic(x) && !is.name(x)) {
    if (is.call(x)) {
      as.character(x[[1]])
    } else {
      unlist(lapply(x, parse_function_body))
    }
  }
}

#' List Available Functions
#'
#' @param env Environment to look in
#' @return Character vector of function names
#' @export
list_available_functions <- function(env = parent.frame()) {
  objs <- ls(envir = env)
  is_function <- sapply(objs, function(x) {
    is.function(get(x, envir = env))
  })
  return(objs[is_function])
}

#' List Functions from Files
#'
#' @param path Path to R files
#' @return Character vector of function names
#' @export
list_file_functions <- function(path) {
  functions <- parse_project_functions(path)
  return(functions$names)
}
