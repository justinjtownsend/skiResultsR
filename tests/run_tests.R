#!/usr/bin/env Rscript

# Simple test runner for skiresultsR package
# Use this if devtools::test() has issues

cat("Loading skiresultsR package...\n")
library(skiresultsR)

cat("Loading testthat...\n")
library(testthat)

cat("Running tests...\n")
test_results <- test_dir("testthat")

cat("\n=== Test Summary ===\n")
print(test_results)
