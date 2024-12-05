library(codenet)

# Create a complex demo project structure
complex_dir <- file.path(tempdir(), "complex_project")
dir.create(file.path(complex_dir, "R"), recursive = TRUE)

# 1. Data Processing Functions
cat('
clean_data <- function(data) {
  data <- remove_missing(data)
  data <- standardize_columns(data)
  return(validate_data(data))
}

remove_missing <- function(data) {
  return(data[complete.cases(data), ])
}

standardize_columns <- function(data) {
  data <- convert_dates(data)
  data <- normalize_numbers(data)
  return(data)
}

convert_dates <- function(data) {
  if("date" %in% names(data)) {
    data$date <- as.Date(data$date)
  }
  return(data)
}

normalize_numbers <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- scale(data[numeric_cols])
  return(data)
}

validate_data <- function(data) {
  if(!check_columns(data)) stop("Invalid columns")
  if(!check_types(data)) stop("Invalid types")
  return(data)
}

check_columns <- function(data) {
  required <- c("id", "value", "date")
  return(all(required %in% names(data)))
}

check_types <- function(data) {
  return(
    is.numeric(data$id) &&
    is.numeric(data$value) &&
    inherits(data$date, "Date")
  )
}
', file = file.path(complex_dir, "R", "data_processing.R"))

# 2. Analysis Functions
cat('
analyze_data <- function(data) {
  clean_data <- clean_data(data)
  results <- list(
    summary = generate_summary(clean_data),
    models = fit_models(clean_data),
    plots = create_plots(clean_data)
  )
  return(compile_report(results))
}

generate_summary <- function(data) {
  return(list(
    basic = basic_stats(data),
    advanced = advanced_stats(data)
  ))
}

basic_stats <- function(data) {
  return(list(
    mean = mean(data$value),
    sd = sd(data$value)
  ))
}

advanced_stats <- function(data) {
  return(list(
    quantiles = quantile(data$value),
    correlation = cor(data$id, data$value)
  ))
}

fit_models <- function(data) {
  return(list(
    linear = fit_linear(data),
    nonlinear = fit_nonlinear(data)
  ))
}

fit_linear <- function(data) {
  return(lm(value ~ id, data = data))
}

fit_nonlinear <- function(data) {
  return(loess(value ~ id, data = data))
}

create_plots <- function(data) {
  return(list(
    time_series = plot_time_series(data),
    scatter = plot_scatter(data)
  ))
}

plot_time_series <- function(data) {
  # Simulate plot creation
  return(data.frame(x = data$date, y = data$value))
}

plot_scatter <- function(data) {
  # Simulate plot creation
  return(data.frame(x = data$id, y = data$value))
}

compile_report <- function(results) {
  return(list(
    summary = format_summary(results$summary),
    model_results = format_models(results$models),
    visualizations = format_plots(results$plots)
  ))
}

format_summary <- function(summary) {
  return(paste("Summary formatted"))
}

format_models <- function(models) {
  return(paste("Models formatted"))
}

format_plots <- function(plots) {
  return(paste("Plots formatted"))
}
', file = file.path(complex_dir, "R", "analysis.R"))

# Create network visualization for the complex project
project_network <- create_function_network(
  source = complex_dir,
  type = "files"
)

# Display the network
project_network

# Clean up
unlink(complex_dir, recursive = TRUE)
