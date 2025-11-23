test_that("get_event works with sample data", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event(file_path)
  
  # Check return type
  expect_type(result, "list")
  expect_s3_class(result, "skiresults_event")
  
  # Check required components
  expected_components <- c("event_dtls", "race_types", "races", "racers", "race_points", "clubs")
  expect_true(all(expected_components %in% names(result)))
  
  # Check component types
  expect_s3_class(result$event_dtls, "data.frame")
  expect_type(result$race_types, "list")
  expect_type(result$races, "list")
  expect_s3_class(result$racers, "data.frame")
  expect_s3_class(result$race_points, "data.frame")
  expect_s3_class(result$clubs, "data.frame")
})

test_that("get_event handles missing file", {
  expect_error(
    get_event("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_event integrates all components correctly", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event(file_path)
  
  # Event details should have title
  expect_true("title" %in% names(result$event_dtls))
  expect_type(result$event_dtls$title, "character")
  
  # Races should be consistent with get_races output
  expect_type(result$races, "list")
  
  # Racers should have proper structure (check what's actually returned)
  expected_racer_cols <- c("name", "href")  # Basic columns that should always exist
  expect_true(all(expected_racer_cols %in% names(result$racers)))
  
  # Club columns may or may not exist depending on data
  if ("club" %in% names(result$racers)) {
    expect_type(result$racers$club, "character")
  }
  
  # Race points should have proper structure (may be empty if no points found)
  expect_s3_class(result$race_points, "data.frame")
  
  # If there are points, check the structure
  if (nrow(result$race_points) > 0) {
    expected_points_cols <- c("name", "bib", "rank", "category", "points", "race_id", "points_category")
    expect_true(all(expected_points_cols %in% names(result$race_points)))
  }
  
  # Even if empty, should have some basic structure (at least be a data frame)
  expect_true(is.data.frame(result$race_points))
  
  # Clubs should have proper structure
  expected_club_cols <- c("name", "href")
  expect_true(all(expected_club_cols %in% names(result$clubs)))
})

test_that("get_event returns consistent data across components", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_event(file_path)
  
  # Race IDs should be consistent between races and race_points
  race_ids_from_races <- names(result$races)
  race_ids_from_points <- unique(result$race_points$race_id)
  race_ids_from_points <- race_ids_from_points[!is.na(race_ids_from_points)]
  
  # Points race IDs should be subset of available races
  if (length(race_ids_from_points) > 0) {
    expect_true(all(race_ids_from_points %in% race_ids_from_races))
  }
  
  # Racer names should be consistent (if any racers found)
  if (nrow(result$racers) > 0 && nrow(result$race_points) > 0) {
    # This is a loose check since not all racers may have points
    # Just ensure no completely inconsistent data
    expect_type(result$racers$name, "character")
    expect_type(result$race_points$name, "character")
  }
})
