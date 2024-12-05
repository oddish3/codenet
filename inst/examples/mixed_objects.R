devtools::load_all(".")
devtools::test()

library(codenet)

# Create a complex environment with different types of objects
# 1. Functions that work with different data structures
list_processor <- function(x) {
  if(is.list(x)) {
    return(recursive_list_process(x))
  }
  return(x)
}

recursive_list_process <- function(lst) {
  lapply(lst, function(x) {
    if(is.list(x)) recursive_list_process(x)
    else process_element(x)
  })
}

process_element <- function(x) {
  if(is.numeric(x)) return(scale_number(x))
  if(is.character(x)) return(clean_string(x))
  return(x)
}

scale_number <- function(x) {
  (x - mean(x)) / sd(x)
}

clean_string <- function(x) {
  trimws(tolower(x))
}

# 2. Data objects
example_data <- list(
  numbers = 1:10,
  text = c("A", "B", "C"),
  nested = list(
    more_numbers = rnorm(5),
    more_text = LETTERS[1:5]
  )
)

# 3. S3 class and methods
create_data_object <- function(data) {
  structure(data, class = "processed_data")
}

print.processed_data <- function(x, ...) {
  cat("Processed Data Object\n")
  str(x)
}

summary.processed_data <- function(object, ...) {
  list_processor(object)
}

# 4. Function that uses both data and other functions
analyze_object <- function(obj) {
  if(inherits(obj, "processed_data")) {
    return(summary.processed_data(obj))
  }
  return(list_processor(obj))
}

# 1. Create network for functions only
function_network <- create_function_network(
  source = c(
    "list_processor", "recursive_list_process", "process_element",
    "scale_number", "clean_string", "create_data_object",
    "print.processed_data", "summary.processed_data", "analyze_object"
  ),
  type = "objects"
)

# 2. Create network for all objects including data
all_objects_network <- create_object_network(
  source = ls(),
  type = "objects",
  object_types = "all"
)

# 3. Create network for just data objects
data_network <- create_object_network(
  source = c("example_data", "test_obj", "processed"),
  type = "objects",
  object_types = "data.frames"
)

# Display the networks
par(mfrow = c(1, 3))
function_network
all_objects_network
data_network
