test_that("get_races works with sample data", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_races(file_path)
  
  # Check return type
  expect_type(result, "list")
  
  # Should find at least one race
  expect_true(length(result) > 0)
  
  # Check structure of first race (should be a data frame from get_race())
  if (length(result) > 0) {
    first_race <- result[[1]]
    expect_s3_class(first_race, "data.frame")
    expect_true(nrow(first_race) >= 0)  # May be empty but should be a data frame
  }
})

test_that("get_races handles missing file", {
  expect_error(
    get_races("nonexistent_file.html"),
    "File does not exist"
  )
})

test_that("get_races extracts race IDs correctly", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_races(file_path)
  
  # Race IDs should start with "race-" and be used as list names
  race_ids <- names(result)
  expect_true(all(grepl("^race-", race_ids)))
  
  # Number of races should match number of race IDs from get_race_types()
  race_types <- get_race_types(file_path)
  race_types_df <- race_types[[1]]$race_types
  expected_race_ids <- race_types_df$race_id[!is.na(race_types_df$race_id) & race_types_df$race_id != ""]
  expect_equal(length(result), length(expected_race_ids))
})

test_that("get_races returns consistent structure", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
  if (!file.exists(file_path) || file_path == "") {
    file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
  }
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_races(file_path)
  
  # All races should be data frames (from get_race())
  for (race in result) {
    expect_s3_class(race, "data.frame")
  }
  
  # Each race should be named by its race_id
  for (race_id in names(result)) {
    expect_true(grepl("^race-", race_id))
    # The race_id should match one from get_race_types()
    race_types <- get_race_types(file_path)
    race_types_df <- race_types[[1]]$race_types
    expect_true(race_id %in% race_types_df$race_id)
  }
})
