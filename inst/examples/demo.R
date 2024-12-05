# File: inst/examples/demo.R
devtools::load_all(".")
devtools::test()

# Create a demo directory with example functions
demo_dir <- file.path(tempdir(), "codenet_demo")
dir.create(demo_dir)

# Write example functions
cat('
# Function to add two numbers
add_numbers <- function(x, y) {
  return(x + y)
}

# Function that uses add_numbers
calculate_sum <- function(numbers) {
  total <- 0
  for(i in numbers) {
    total <- add_numbers(total, i)
  }
  return(total)
}

# Function that uses both
process_data <- function(data) {
  result <- calculate_sum(data)
  return(list(
    sum = result,
    mean = result/length(data)
  ))
}
', file = file.path(demo_dir, "functions.R"))

# Create and view the network
network <- create_function_network(demo_dir)
network

# Clean up
unlink(demo_dir, recursive = TRUE)
