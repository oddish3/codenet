# Set up test environment before tests
withr::local_dir(tempdir())

# Create test files
setup_test_environment <- function() {
  test_dir <- test_path("example_project")
  dir.create(file.path(test_dir, "R"), recursive = TRUE, showWarnings = FALSE)

  # Create example R file
  cat('
add_numbers <- function(x, y) {
  return(x + y)
}

multiply_numbers <- function(x, y) {
  return(x * y)
}

calculate_total <- function(x, y) {
  sum <- add_numbers(x, y)
  product <- multiply_numbers(x, y)
  return(list(sum = sum, product = product))
}
', file = file.path(test_dir, "R", "math_functions.R"))

  return(test_dir)
}

# Run setup before all tests
test_dir <- setup_test_environment()

test_that("Function network creation works with files", {
  # Test network creation
  network <- create_function_network(
    source = test_dir,
    type = "files"
  )

  # Check that network is created correctly
  expect_s3_class(network, "visNetwork")

  # Check that all functions are included
  expected_functions <- c("add_numbers", "multiply_numbers", "calculate_total")
  actual_functions <- unique(c(network$x$nodes$id))
  expect_setequal(actual_functions, expected_functions)

  # Check dependencies
  edges <- network$x$edges
  expect_true(any(edges$from == "calculate_total" & edges$to == "add_numbers"))
  expect_true(any(edges$from == "calculate_total" & edges$to == "multiply_numbers"))
})

test_that("Function network creation works with objects", {
  # Create some functions in the environment
  add <- function(x, y) x + y
  multiply <- function(x, y) x * y
  calculate <- function(x, y) {
    sum <- add(x, y)
    prod <- multiply(x, y)
    return(list(sum = sum, prod = prod))
  }

  # Test network creation
  network <- create_function_network(
    source = c("add", "multiply", "calculate"),
    type = "objects"
  )

  # Check that network is created correctly
  expect_s3_class(network, "visNetwork")

  # Check that all functions are included
  expected_functions <- c("add", "multiply", "calculate")
  actual_functions <- unique(c(network$x$nodes$id))
  expect_setequal(actual_functions, expected_functions)

  # Check dependencies
  edges <- network$x$edges
  expect_true(any(edges$from == "calculate" & edges$to == "add"))
  expect_true(any(edges$from == "calculate" & edges$to == "multiply"))
})

test_that("Function parsing works correctly for files", {
  functions <- parse_project_functions(test_dir)

  expect_type(functions, "list")
  expect_named(functions, c("names", "env"))
  expect_true(all(c("add_numbers", "multiply_numbers", "calculate_total") %in% functions$names))
})

test_that("Function parsing works correctly for objects", {
  # Create test functions
  add <- function(x, y) x + y
  multiply <- function(x, y) x * y

  functions <- parse_memory_functions(c("add", "multiply"), environment())

  expect_type(functions, "list")
  expect_named(functions, c("names", "env"))
  expect_true(all(c("add", "multiply") %in% functions$names))
})

test_that("Dependency identification works", {
  functions <- parse_project_functions(test_dir)
  edges <- identify_function_dependencies(functions)

  expect_s3_class(edges, "data.frame")
  expect_named(edges, c("from", "to"))
  expect_true(nrow(edges) >= 2)  # calculate_total should call both other functions
})

test_that("list_available_functions works", {
  # Create test functions
  add <- function(x, y) x + y
  multiply <- function(x, y) x * y
  not_a_function <- 42

  funcs <- list_available_functions()

  expect_type(funcs, "character")
  expect_true(all(c("add", "multiply") %in% funcs))
  expect_false("not_a_function" %in% funcs)
})

test_that("list_file_functions works", {
  funcs <- list_file_functions(test_dir)

  expect_type(funcs, "character")
  expect_true(all(c("add_numbers", "multiply_numbers", "calculate_total") %in% funcs))
})

test_that("Error handling works correctly", {
  # Test invalid type
  expect_error(
    create_function_network(source = ".", type = "invalid"),
    "should be one of"
  )

  # Test non-existent directory
  expect_error(
    create_function_network(source = "nonexistent", type = "files"),
    "does not exist"
  )

  # Test invalid object names
  expect_error(
    create_function_network(source = "nonexistent_object", type = "objects"),
    "not found"
  )
})

# Clean up after all tests
withr::defer({
  unlink(test_dir, recursive = TRUE)
})
