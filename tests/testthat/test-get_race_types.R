test_that("get_race_types works with brentwood sample data", {
  # Get sample file path - try system.file first, then direct path
  file_path <- system.file("extdata", "brentwood_jun2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "brentwood_jun2023.html")
  }
  
  # Skip if file doesn't exist
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  # Test basic functionality
  result <- get_race_types(file_path)
  
  # Check return type
  expect_type(result, "list")
  expect_length(result, 1)
  
  # Check that list is named with event_id
  expect_true(length(names(result)) > 0)
  event_id <- names(result)[1]
  expect_false(is.na(event_id))
  expect_true(is.character(event_id))
  expect_true(nchar(event_id) > 0)
  expect_true(grepl("^\\d+$", event_id), "Event ID should be numeric")
  
  # Check for race_types data frame
  expect_true("race_types" %in% names(result[[1]]))
  race_types <- result[[1]]$race_types
  
  expect_s3_class(race_types, "data.frame")
  expect_true(nrow(race_types) > 0)
  
  # Check required columns
  expect_true("event_id" %in% names(race_types))
  expect_true("race_type" %in% names(race_types))
  expect_true("race_id" %in% names(race_types))
  
  # Check that event_id matches the list name
  expect_equal(unique(race_types$event_id), event_id)
  
  # Check that race_type values are not empty
  expect_true(all(nchar(race_types$race_type) > 0))
  
  # Check that race_id values follow expected pattern (race-XXXX or NA)
  valid_race_ids <- race_types$race_id[!is.na(race_types$race_id)]
  if (length(valid_race_ids) > 0) {
    expect_true(all(grepl("^race-\\d+$", valid_race_ids)))
  }
})

test_that("get_race_types works with chatham sample data", {
  # Get sample file path - try system.file first, then direct path
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  
  # Skip if file doesn't exist
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  # Test basic functionality
  result <- get_race_types(file_path)
  
  # Check return type
  expect_type(result, "list")
  expect_length(result, 1)
  
  # Check that list is named with event_id
  expect_true(length(names(result)) > 0)
  event_id <- names(result)[1]
  expect_false(is.na(event_id))
  expect_true(is.character(event_id))
  expect_true(nchar(event_id) > 0)
  expect_true(grepl("^\\d+$", event_id), "Event ID should be numeric")
  
  # Check for race_types data frame
  expect_true("race_types" %in% names(result[[1]]))
  race_types <- result[[1]]$race_types
  
  expect_s3_class(race_types, "data.frame")
  expect_true(nrow(race_types) > 0)
  
  # Check required columns
  expect_true("event_id" %in% names(race_types))
  expect_true("race_type" %in% names(race_types))
  expect_true("race_id" %in% names(race_types))
  
  # Check that event_id matches the list name
  expect_equal(unique(race_types$event_id), event_id)
  
  # Check that race_type values are not empty
  expect_true(all(nchar(race_types$race_type) > 0))
  
  # Check that race_id values follow expected pattern (race-XXXX or NA)
  valid_race_ids <- race_types$race_id[!is.na(race_types$race_id)]
  if (length(valid_race_ids) > 0) {
    expect_true(all(grepl("^race-\\d+$", valid_race_ids)))
  }
  
  # Chatham should have "Individual" race type
  expect_true("Individual" %in% race_types$race_type)
})

test_that("get_race_types handles missing file", {
  expect_error(
    get_race_types("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_race_types extracts correct event IDs", {
  # Test brentwood
  file_path_brentwood <- system.file("extdata", "brentwood_jun2023.html", package = "skiResultsR")
  if (!file.exists(file_path_brentwood) || file_path_brentwood == "") {
    file_path_brentwood <- file.path("inst", "extdata", "brentwood_jun2023.html")
  }
  skip_if_not(file.exists(file_path_brentwood), "Brentwood HTML file not found")
  
  result_brentwood <- get_race_types(file_path_brentwood)
  expect_equal(names(result_brentwood)[1], "1316")
  
  # Test chatham
  file_path_chatham <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path_chatham) || file_path_chatham == "") {
    file_path_chatham <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path_chatham), "Chatham HTML file not found")
  
  result_chatham <- get_race_types(file_path_chatham)
  expect_equal(names(result_chatham)[1], "1319")
})

