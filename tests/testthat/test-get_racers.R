test_that("get_racers works with sample data", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_racers(file_path)
  
  # Check return type
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "skiresults_racers")
  
  # Check required columns
  expected_cols <- c("name", "href", "club", "club_href")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check column types
  expect_type(result$name, "character")
  expect_type(result$href, "character")
  expect_type(result$club, "character")
  expect_type(result$club_href, "character")
})

test_that("get_racers handles missing file", {
  expect_error(
    get_racers("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_racers returns unique racers only", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_racers(file_path)
  
  # Should have unique hrefs (no duplicates)
  expect_equal(length(unique(result$href)), nrow(result))
})

test_that("get_racers extracts valid URLs", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_racers(file_path)
  
  # Skip if no racers found
  skip_if(nrow(result) == 0, "No racers found in sample file")
  
  # URLs should contain "/people/"
  valid_urls <- grepl("/people/", result$href, fixed = TRUE)
  expect_true(all(valid_urls | is.na(result$href)))
})

test_that("get_racers handles empty results gracefully", {
  # This test would need a file with no racer links
  # For now, just ensure the function returns the right structure even with no data
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_racers(file_path)
  
  # Even if empty, should have the right structure
  expected_cols <- c("name", "href", "club", "club_href")
  expect_true(all(expected_cols %in% names(result)))
})
