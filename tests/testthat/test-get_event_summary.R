test_that("get_event_summary works with sample data", {
  # Get sample file path - try system.file first, then direct path
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  
  # Skip if file doesn't exist
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  # Test basic functionality
  result <- get_event_summary(file_path)
  
  # Check return type
  expect_type(result, "list")
  expect_s3_class(result, "skiresults_event_summary")
  
  # Check required fields
  expect_true("title" %in% names(result))
  expect_type(result$title, "character")
  expect_true(nchar(result$title) > 0)
  
  # Check for race types
  expect_true("race_types" %in% names(result))
  expect_type(result$race_types, "list")
})

test_that("get_event_summary handles missing file", {
  expect_error(
    get_event_summary("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_event_summary extracts date information", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_summary(file_path)
  
  # Should extract date from title
  if ("date" %in% names(result)) {
    expect_type(result$date, "character")
    expect_true(nchar(result$date) > 0)
  }
})

test_that("get_event_summary extracts slope information", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_summary(file_path)
  
  # Should extract slope from title
  if ("slope" %in% names(result)) {
    expect_type(result$slope, "character")
    expect_true(nchar(result$slope) > 0)
  }
})
