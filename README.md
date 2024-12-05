# codenet

<!-- badges: start -->
<!-- badges: end -->

A package for visualizing code dependencies and object relationships in R projects.


## Installation

You can install the development version of codenet from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("oddish3/codenet")
```
or 
``` r
# install.packages("devtools")
devtools::install_github("yourusername/codenet")
```

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(codenet)

# Create function network
function_network <- create_function_network(
  source = ls(),
  type = "objects"
)

# Create object network
object_network <- create_object_network(
  source = ls(),
  type = "objects",
  object_types = "all"
)
```
## Examples

Say we write a function to add two numbers:
```
add_numbers <- function(x, y) {
  return(x + y)
}
```

then we write another function that uses this function to calculate a total:
```
calculate_sum <- function(numbers) {
  total <- 0
  for(i in numbers) {
    total <- add_numbers(total, i)
  }
  return(total)
}
```

And then we write another function that uses the `calculate_sum` function:
```
process_data <- function(data) {
  result <- calculate_sum(data)
  return(list(
    sum = result,
    mean = result/length(data)
  ))
}
```

We can easily visualise the dependecies of these functions in the ![Rstudio Viewer Pane](https://github.com/user-attachments/assets/286e09c3-6b08-43a2-b093-257a6e0115b7)
