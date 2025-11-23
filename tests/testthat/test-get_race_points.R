test_that("get_race_points works with sample data", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race_points(file_path)
  
  # Check return type
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "skiresults_points")
  
  # Check required columns
  expected_cols <- c("name", "bib", "rank", "category", "points", "race_id", "points_category")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check column types
  expect_type(result$name, "character")
  expect_type(result$bib, "character")
  expect_type(result$rank, "character")
  expect_type(result$category, "character")
  expect_type(result$points, "double")
  expect_type(result$race_id, "character")
  expect_type(result$points_category, "character")
})

test_that("get_race_points handles missing file", {
  expect_error(
    get_race_points("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_race_points handles files with no points", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race_points(file_path)
  
  # Even if no points found, should return proper structure
  expected_cols <- c("name", "bib", "rank", "category", "points", "race_id", "points_category")
  expect_true(all(expected_cols %in% names(result)))
  
  # If points exist, they should be numeric
  if (nrow(result) > 0) {
    expect_type(result$points, "double")
    expect_true(all(is.numeric(result$points) | is.na(result$points)))
  }
})

test_that("get_race_points extracts valid race IDs", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race_points(file_path)
  
  # Skip if no points found
  skip_if(nrow(result) == 0, "No points found in sample file")
  
  # Race IDs should start with "race-"
  expect_true(all(grepl("^race-", result$race_id) | is.na(result$race_id)))
})

test_that("get_race_points handles points categories correctly", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race_points(file_path)
  
  # Skip if no points found
  skip_if(nrow(result) == 0, "No points found in sample file")
  
  # Points categories should be character strings
  expect_type(result$points_category, "character")
  
  # Categories should be non-empty strings (or NA)
  non_na_categories <- result$points_category[!is.na(result$points_category)]
  if (length(non_na_categories) > 0) {
    expect_true(all(nchar(non_na_categories) > 0))
  }
})
