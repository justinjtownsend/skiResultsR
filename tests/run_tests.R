#!/usr/bin/env Rscript

# Simple test runner for skiResultsR package
# Use this if devtools::test() has issues

cat("Loading skiResultsR package...\n")
library(skiResultsR)

cat("Loading testthat...\n")
library(testthat)

cat("Running tests...\n")
test_results <- test_dir("testthat")

cat("\n=== Test Summary ===\n")
print(test_results)
