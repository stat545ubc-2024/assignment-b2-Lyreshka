# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(summarizePlotR) ## no ggplot2 wooooooo!


### mock data
data <- iris
x_var <- "Species"
y_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
plot_types <- c("box", "density")

test_that("summarize_and_plot returns correct output", {
  result <- summarize_and_plot(data, x_var, y_vars, plot_types)

  # Check if result is a list with expected elements
  expect_type(result, "list")
  expect_true("summary_table" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_s3_class(result$summary_table, "data.frame")
  expect_type(result$plots, "list")
})

test_that("summarize_and_plot generates the specified plot types", {
  result <- summarize_and_plot(data, x_var, y_vars, plot_types)

  # Check that the correct plots are created for each plot type and y_var (specified)
  expect_true(all(paste0("box_", y_vars) %in% names(result$plots)))
  expect_true(all(paste0("density_", y_vars) %in% names(result$plots)))
})

test_that("summarize_and_plot creates combined plots when specified", {
  result <- summarize_and_plot(data, x_var, y_vars, plot_types, combined_plot = TRUE)

  # Check that combined plots are named correctly
  expect_true("Combined_box" %in% names(result$plots))
  expect_true("Combined_density" %in% names(result$plots))
})

test_that("summarize_and_plot summarizes data correctly", {
  data <- iris
  x_var <- "Species"
  y_vars <- c("Sepal.Length", "Sepal.Width")

  result <- summarize_and_plot(data, x_var, y_vars, plot_types = c())

  # Check summary table values
  summary_table <- result$summary_table

  # Verify the expected columns
  expect_true(all(paste0(y_vars, "_mean") %in% colnames(summary_table)))
  expect_true(all(paste0(y_vars, "_median") %in% colnames(summary_table)))
  expect_true(all(paste0(y_vars, "_sd") %in% colnames(summary_table)))
  expect_true(all(paste0(y_vars, "_max") %in% colnames(summary_table)))
  expect_true(all(paste0(y_vars, "_min") %in% colnames(summary_table)))
})

### this is specialized due to my need to test misspelled variables
test_that("summarize_and_plot handles invalid inputs gracefully", {
  # Use a nonexistent y variable in the iris dataset to trigger the error
  data <- iris
  x_var <- "Species"
  invalid_y_vars <- c("NonexistentVar")
  plot_types <- c("box")

  # Expect an error due to invalid y_vars, and match the specific error message
  expect_error(
    summarize_and_plot(data, x_var, invalid_y_vars, plot_types),
    "The following y variables do not exist in the dataset: NonexistentVar"
  )
})


