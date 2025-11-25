test_that("get_event_dtls works with sample data", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_dtls(file_path)
  
  # Check return type
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  
  # Check required columns
  expected_cols <- c("title", "date", "slope", "slope_url", "format", "status")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check column types
  expect_type(result$title, "character")
  expect_type(result$date, "character")
  expect_type(result$slope, "character")
  expect_type(result$slope_url, "character")
  expect_type(result$format, "character")
  expect_type(result$status, "character")
})

test_that("get_event_dtls extracts title correctly", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_dtls(file_path)
  
  # Title should be extracted from head/title
  expect_true(!is.na(result$title))
  expect_true(nchar(result$title) > 0)
  expect_true(grepl("Chatham", result$title, ignore.case = TRUE))
})

test_that("get_event_dtls extracts date in ISO format", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_dtls(file_path)
  
  # Date should be in ISO format (YYYY-MM-DD)
  if (!is.na(result$date)) {
    expect_true(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", result$date))
    # Should be 2023-10-07 for October 7, 2023
    expect_true(grepl("2023-10-07", result$date))
  }
})

test_that("get_event_dtls extracts slope information", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_dtls(file_path)
  
  # Slope should be extracted
  expect_true(!is.na(result$slope))
  expect_true(nchar(result$slope) > 0)
  expect_true(grepl("Chatham", result$slope, ignore.case = TRUE))
  
  # Slope URL should be a valid URL
  if (!is.na(result$slope_url)) {
    expect_true(grepl("^https?://", result$slope_url))
    expect_true(grepl("skiresults.co.uk", result$slope_url))
  }
})

test_that("get_event_dtls extracts format and status", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_dtls(file_path)
  
  # Format should be extracted
  expect_true(!is.na(result$format))
  expect_true(nchar(result$format) > 0)
  
  # Status should be extracted
  expect_true(!is.na(result$status))
  expect_true(nchar(result$status) > 0)
})

test_that("get_event_dtls handles missing file", {
  expect_error(
    get_event_dtls("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_event_dtls returns consistent structure", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event_dtls(file_path)
  
  # Should always return exactly 1 row
  expect_equal(nrow(result), 1)
  
  # Should always have exactly 6 columns
  expect_equal(ncol(result), 6)
  
  # Column order should be consistent
  expect_equal(names(result), c("title", "date", "slope", "slope_url", "format", "status"))
})


