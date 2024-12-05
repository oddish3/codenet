library(codenet)

# Example 1: Working with R objects in memory
add_numbers <- function(x, y) x + y
calculate_sum <- function(numbers) {
  total <- 0
  for(i in numbers) total <- add_numbers(total, i)
  return(total)
}
process_data <- function(data) {
  result <- calculate_sum(data)
  return(list(sum = result, mean = result/length(data)))
}

# Create network from objects in memory
obj_network <- create_function_network(
  source = c("add_numbers", "calculate_sum", "process_data"),
  type = "objects"
)
obj_network

# Example 2: Working with R files in a project
project_network <- create_function_network(
  source = "path/to/your/code",
  type = "files"
)
project_network
