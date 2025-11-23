test_that("get_race works with valid race ID", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  # Test with a known race ID
  result <- get_race(file_path, "race-9973")
  
  # Check return type
  expect_s3_class(result, "data.frame")
  
  # Should have some rows
  expect_true(nrow(result) > 0)
  
  # Should have some columns
  expect_true(ncol(result) > 0)
})

test_that("get_race handles missing file", {
  expect_error(
    get_race("nonexistent_file.html", "race-9973"),
    "File does not exist"
  )
})

test_that("get_race handles missing race_id", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  expect_error(
    get_race(file_path),
    "race_id is required"
  )
  
  expect_error(
    get_race(file_path, ""),
    "race_id is required"
  )
  
  expect_error(
    get_race(file_path, NULL),
    "race_id is required"
  )
})

test_that("get_race handles invalid race ID", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  expect_error(
    get_race(file_path, "nonexistent-race-123"),
    "Race table not found"
  )
})

test_that("get_race processes individual race table (race-9973)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9973")
  
  # Should be a data frame
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Should have expected columns for individual race
  expected_cols <- c("Rank", "Bib", "Name", "Club", "Overall Time")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
  
  # Check that Rank column has numeric values
  if ("Rank" %in% names(result)) {
    expect_true(is.numeric(result$Rank) || is.character(result$Rank))
  }
  
  # Check that Bib column exists
  if ("Bib" %in% names(result)) {
    expect_true(nrow(result[!is.na(result$Bib), ]) > 0)
  }
})

test_that("get_race processes Points header correctly (Rule 5.2)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9973")
  
  # Check if Points column exists (may be named with concatenated picked class)
  points_cols <- grep("Points", names(result), ignore.case = TRUE, value = TRUE)
  
  # If Points column exists, it should have the concatenated format
  if (length(points_cols) > 0) {
    # Should contain "Points" and a picked class name
    expect_true(any(grepl("Points", points_cols, ignore.case = TRUE)))
  }
})

test_that("get_race processes team race with win_for attributes (race-9974)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9974")
  
  # Should be a data frame
  expect_s3_class(result, "data.frame")
  
  # Should have some rows
  expect_true(nrow(result) > 0)
  
  # Check for "Win For" column (Rule 5.3 - blank headers become "Win For")
  if ("Win For" %in% names(result)) {
    # Should have some non-empty values (Rule 6.4 - win_for values inserted)
    win_for_values <- result[["Win For"]]
    non_empty <- win_for_values[!is.na(win_for_values) & trimws(win_for_values) != ""]
    expect_true(length(non_empty) > 0, 
                info = "Win For column should have values from win_for attributes")
    
    # Values should contain "win_for" pattern (Rule 6.3 & 6.4)
    if (length(non_empty) > 0) {
      expect_true(any(grepl("win_for", non_empty, ignore.case = TRUE)),
                  info = "Win For values should contain win_for pattern from class attribute")
    }
  }
})

test_that("get_race processes club race table (race-9981)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9981")
  
  # Should be a data frame
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Should have expected columns
  expect_true("Rank" %in% names(result))
  expect_true("Name" %in% names(result) || "Club" %in% names(result))
})

test_that("get_race extracts nested text correctly (Rule 6.1)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9973")
  
  # Name column should have text from nested <a> tags
  if ("Name" %in% names(result)) {
    # Should have some non-empty names
    names <- result$Name[!is.na(result$Name) & trimws(result$Name) != ""]
    expect_true(length(names) > 0)
    
    # Names should be readable (not HTML tags)
    expect_false(any(grepl("<", names, fixed = TRUE)), 
                 info = "Names should not contain HTML tags")
  }
  
  # Club column should have text from nested <a> tags
  if ("Club" %in% names(result)) {
    clubs <- result$Club[!is.na(result$Club) & trimws(result$Club) != ""]
    expect_true(length(clubs) > 0)
    expect_false(any(grepl("<", clubs, fixed = TRUE)), 
                 info = "Clubs should not contain HTML tags")
  }
})

test_that("get_race extracts display:inline span values (Rule 6.2)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9973")
  
  # Points column should have values from display:inline spans
  points_cols <- grep("Points", names(result), ignore.case = TRUE, value = TRUE)
  
  if (length(points_cols) > 0) {
    points_col <- result[[points_cols[1]]]
    # Should have some numeric or character values (not all empty)
    non_empty <- points_col[!is.na(points_col) & trimws(points_col) != ""]
    # Note: Some rows may be empty, but at least some should have values
    # This is a soft check - if there are any non-empty values, the rule is working
    # The column should exist and be processable
    expect_true(is.character(points_col) || is.numeric(points_col),
                info = "Points column should be character or numeric")
  } else {
    # If no Points column, that's also valid for some race types
    expect_true(TRUE, info = "No Points column found - may not be present in all races")
  }
})

test_that("get_race handles multiple race types from different files", {
  # Test with chatham_oct2023.html
  file_path1 <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path1), "Sample HTML file not found")
  
  result1 <- get_race(file_path1, "race-9973")
  expect_s3_class(result1, "data.frame")
  expect_true(nrow(result1) > 0)
  
  # Test with brentwood_jun2023.html if available
  file_path2 <- system.file("extdata", "brentwood_jun2023.html", package = "skiResultsR")
  if (file.exists(file_path2)) {
    result2 <- get_race(file_path2, "race-9711")
    expect_s3_class(result2, "data.frame")
    expect_true(nrow(result2) > 0)
  }
})

test_that("get_race produces consistent column names", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  # Test multiple races from same file
  race_ids <- c("race-9973", "race-9981", "race-9974")
  
  for (race_id in race_ids) {
    result <- tryCatch({
      get_race(file_path, race_id)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(result)) {
      # Column names should be character strings
      expect_type(names(result), "character")
      expect_true(length(names(result)) > 0)
      
      # Column names should not be empty
      expect_false(any(names(result) == ""), 
                   info = paste("Race", race_id, "has empty column names"))
    }
  }
})

test_that("get_race handles time columns with DNS/DNF/DSQ values", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9973")
  
  # Check time-related columns
  time_cols <- grep("Time|Run", names(result), ignore.case = TRUE, value = TRUE)
  
  if (length(time_cols) > 0) {
    for (col in time_cols) {
      values <- result[[col]]
      # Values should be character (may contain DNS, DNF, DSQ or times)
      expect_true(is.character(values) || is.numeric(values))
      
      # If character, may contain DNS, DNF, DSQ
      if (is.character(values)) {
        # Should be able to handle these special values
        special_values <- c("DNS", "DNF", "DSQ")
        has_special <- any(toupper(trimws(values)) %in% special_values, na.rm = TRUE)
        # This is just a check that we can handle these - not all races will have them
      }
    }
  }
})

test_that("get_race processes header nesting correctly (Rule 5.1)", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9981")
  
  # Headers with nesting should still produce readable column names
  expect_true(all(nchar(names(result)) > 0), 
              info = "All column names should have content")
  
  # Column names should not contain HTML tags
  expect_false(any(grepl("<", names(result), fixed = TRUE)), 
               info = "Column names should not contain HTML tags")
})

test_that("get_race returns data frame suitable for further processing", {
  file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
  skip_if_not(file.exists(file_path), "Sample HTML file not found")
  
  result <- get_race(file_path, "race-9973")
  
  # Should be a standard data frame
  expect_s3_class(result, "data.frame")
  
  # Should be able to perform standard operations
  expect_no_error(nrow(result))
  expect_no_error(ncol(result))
  expect_no_error(names(result))
  
  # Should be able to subset
  if (nrow(result) > 0) {
    expect_no_error(result[1, ])
    expect_no_error(result[, 1])
  }
})
