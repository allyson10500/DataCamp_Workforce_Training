# Create a package skeleton named unitConverter
create_package("unitConverter")

# View and check the R package directory
dir(".")

# Read in distance data
distance_data <- read_csv("data-raw/distance_data.csv")

# Print the first six rows of distance_data
head(distance_data, 6)

# Add the distance data to the R package
use_data(distance_data)

# Make an R Markdown template file
use_rmarkdown_template("Distance Conversion Examples")

# Define the dist_converter function
dist_converter <- function(dist_value, unit_from, unit_to) {
  # Fill in the missing parts of the if-else if-else flow
  if (unit_from == "feet" && unit_to == "meters") {
    return(dist_value / 3.28)
  } else if (unit_from == "meters" && unit_to == "feet") {
    return(dist_value * 3.28)
  } else if (unit_from == unit_to) {
    warning("unit_from and unit_to are the same, returning dist_value")
    return(dist_value)
  } else {
    stop("This function only supports conversions between feet and meters.")
  }
}

# Use dist_converter to convert 100 meters to feet
dist_converter(100, "meters", "feet")

# Save the dist_converter function to an R file using base R
dump("dist_converter", file = "R/dist_converter.R")

#What code installs the unitConverter package, assuming you are in the package working directory?
devtools::install()

# Call the dist_converter function from the unitConverter package
unitConverter:::dist_converter(dist_value = distance_data$value[2], 
                               # Access the second row for unit
                               unit_from = distance_data$unit[2], 
                               unit_to = "feet")

# Check if gg_plot2 meets CRAN naming standards
library(available)
available("gg_plot2")

# Check if unitConverter is available on CRAN
available("unitConverter")

# Check sentiment of terrible
available("terrible")

# Use the appropriate license
use_mit_license()

# Find appropriate pounds
weight_converter(100, unit_from = "kilogram", unit_to = "pound")

# Save the weight_converter function to an R file
dump("weight_converter", file = "R/weight_converter.R")

# Import package files to current R session
library(devtools)
load_all()

# Review the function definition, and fix the syntax error
weight_converter <- function(value, from_unit, to_unit) {
  conversion_factors <- c(gram = 1, kilogram = 1000,
                          pound = 453.592, ounce = 28.3495)
  value_in_gram <- value * conversion_factors[from_unit]
  return(value_in_gram / conversion_factors[to_unit])
}

# Dump the fixed function code to the file
dump("weight_converter", "R/weight_converter.R")

# Load the files once more
load_all()

# Suggest ggplot2 >= 3.0.0
use_package("ggplot2", type = "Suggests", min_version = "3.0.0")

# Import dplyr >= 1.1.0
use_package("dplyr", type = "Imports", min_version = "1.1.0")

# Add the title
#' Convert between distances
#'
# Add the description
#' Performs the conversion based on specified unit_from and unit_to values.
#'
# Add appropriate tag and details to document the first argument
#' @param A numerical distance value to be converted.
dist_converter <- function(dist_value, unit_from, unit_to) {
  if (unit_from == "feet" && unit_to == "meters") {
    return(dist_value / 3.28)
  } else if (unit_from == "meters" && unit_to == "feet") {
    return(dist_value * 3.28)
  } else if (unit_from == unit_to) {
    warning("unit_from and unit_to are the same, returning dist_value")
    return(dist_value)
  } else {
    stop("This function only supports conversions between feet and meters.")
  }
}


#' Convert between distances
#'
#' Performs the conversion based on specified `unit_from` and `unit_to` values.
#'
#' @param A numerical distance value to be converted.
#' @param A character string of the distance unit to convert from 
#' @param A character string of the distance unit to convert to
# Add returning value description and tag
#' @returns A numeric distance value in the unit specified as `unit_to`.
# Export this function
#' @export
dist_converter <- function(dist_value, unit_from, unit_to) {
  if (unit_from == "feet" && unit_to == "meters") {
    return(dist_value / 3.28)
  } else if (unit_from == "meters" && unit_to == "feet") {
    return(dist_value * 3.28)
  } else if (unit_from == unit_to) {
    warning("unit_from and unit_to are the same, returning dist_value")
    return(dist_value)
  } else {
    stop("This function only supports conversions between feet and meters.")
  }
}

# Generate all function documentation
library(roxygen2)
roxygenize()

# Browse vignettes for ggplot2 package
browseVignettes("ggplot2")

# Create vignette
library(usethis)
use_vignette(name = "dist_conversion", 
             title = "Distance Conversion Examples" )

___
title: "Distance Conversion Examples"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Distance Conversion Examples}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
___

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

```{r setup}
library(unitConverter)
```

# Convert feet to meters

We can use the package to convert 5 feet into meters as follows.

```{r dist-example}
dist_result <- dist_converter(dist_value = 5, unit_from = "feet", unit_to = "meters")
```

Five feet is `r dist_result` meters.

# Build all package vignettes
devtools::build_vignettes()

# Create testing framework template
library(usethis)
use_testthat()

# Create test file for dist_converter
use_test(name = "dist_converter")

# Review the example
dist_converter(100, unit_from = "meters", unit_to = "feet")

# Convert the example to a unit test comparing sameness
expect_identical(dist_converter(100, "meters", "feet"), dist_converter(100, "meters", "feet"))

# Warning for feet to feet
expect_warning(dist_converter(25, "feet", "feet"))

# Error for cold feet
expect_error(dist_converter("cold", "feet", "meters"))

# Change each of the expectations below to have correct outputs
expect_equal(
  dist_converter(200, unit_from = "meters", unit_to = "feet"),
  656
)
expect_equal(
  dist_converter(656, unit_from = "feet", unit_to = "meters"),
  200
)

# Wrap working expectations together in a group
library(testthat)
test_that("Conversion from feet to meters and meters to feet works"
,{
  expect_equal(dist_converter(200, unit_from = "meters", 
                                unit_to = "feet"),
               656)
  expect_equal(dist_converter(656, unit_from = "feet", 
                              unit_to = "meters"),
               200)
})

# Test dist_converter() examples
test_example("man/dist_converter.Rd")

# Run all package tests
test_package("unitConverter")

Package: unitConverter  
Title: Unit Conversion Utilities for Distance, Time, Weight, and Temperature
Version: 0.1.0
Authors@R: 
    person("R", "User", , 
           "r.user@cran.com", 
           role = c("aut", "cre"))
Description: The package provides a collection of utility functions for converting distance, time, weight, and temperature values.
License: CC0  
Encoding: UTF-8  
Roxygen: list(markdown = TRUE)  
RoxygenNote: 7.2.3  
Depends:  
    R (>= 2.10)  
LazyData: true  
Suggests:   
    ggplot2 (>= 3.0.0),  
    tibble,  
    knitr,  
    rmarkdown,  
    testthat (>= 3.0.0)  
VignetteBuilder: knitr  
Config/testthat/edition: 3

# Add header and description
#' Distance values and units
#'
#' Distance values and corresponding unit (feet, meters, or inches)
# Update format
#' @format Data frame with two columns and 1000 rows
# Document column names and information
#' \describe{
#' \item{value}{Numeric distance value}
#' \item{unit}{Distance unit}
#' }
# Add example and string of data name
#' @examples
#'   distance_data
"distance_data"

# Assess the package
devtools::check()










