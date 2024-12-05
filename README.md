
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

See the inst/examples directory for detailed examples.
