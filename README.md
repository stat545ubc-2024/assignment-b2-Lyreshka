---
output: github_document
---

The `summarize_and_plot` function generates summary statistics (mean, median, standard deviation, max, min) of specified numeric columns grouped by one or more variables, and creates plots (e.g., box, scatter, density, histogram, or bar plots) for each column or as combined plots, depending on the specified combined_plot argument. I designed the `summarize_and_plot`function to be a quick and efficient data exploration tool, that allows for an overview of key trends before a more in-depth analysis. Additionally, the reasoning behind the name is that it combines summary statistics and customization of basics plots rapidly asses relationships.


```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# summarizePlotR

<!-- badges: start -->
<!-- badges: end -->

`summarizePlotR` is an R package designed to streamline the process of creating exploratory summary statistics and visualizations for categorical and continuous variables in your datasets. Ideal for initial data exploration, it provides an efficient way to inspect relationships between variables through summary tables and plots.  

## Installation

You can install the development version of summarizePlotR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("org/repo", ref = "0.1.0")

```
# Usage

The summarize_and_plot function creates summary statistics and generates various plot types, such as boxplots, scatter plots, and density plots, based on specified inputs. This allows for quick exploration of data with customizable plot options for a selected categorical variable (x_var) and multiple continuous variables (y_vars).

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
# Load package
library(summarizePlotR)

# Example data
data <- iris
x_var <- "Species"
y_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
plot_types <- c("box", "scatter")

# summary and plots
result <- summarize_and_plot(data, x_var, y_vars, plot_types)

# View summary table
print(result$summary_table)

# Display 
result$plots$box_Sepal.Length
result$plots$scatter_Sepal.Width
```

# Features

Summary Statistics: Produces a summary table with the mean, median, standard deviation, minimum, and maximum values for each y_var.

Plotting Options: Choose from box plots, scatter plots, density plots, and combined plots for flexible visualization.

Error Handling: Ensures input consistency, providing informative messages for any invalid input parameters. The `summary_and_plot` function has built in data cleaning such as mutating to numeric values and establishing the presence of the variables called. This package is suitable for users who need a quick, standardized method for data inspection and preliminary analysis. 

