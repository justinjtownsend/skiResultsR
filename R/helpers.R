#' Clean and standardize race times
#' 
#' @param time_str Character string containing race time
#' @return Numeric time in seconds or NA if DNF/DNS
#' @export
clean_race_time <- function(time_str) {
  if (is.null(time_str) || is.na(time_str)) return(NA)
  
  # Handle DNF/DNS cases
  if (toupper(time_str) %in% c("DNF", "DNS")) return(NA)
  
  # Split time into components
  parts <- strsplit(time_str, ":")[[1]]
  
  if (length(parts) == 2) {
    # Format: MM:SS
    minutes <- as.numeric(parts[1])
    seconds <- as.numeric(parts[2])
    return(minutes * 60 + seconds)
  } else if (length(parts) == 3) {
    # Format: HH:MM:SS
    hours <- as.numeric(parts[1])
    minutes <- as.numeric(parts[2])
    seconds <- as.numeric(parts[3])
    return(hours * 3600 + minutes * 60 + seconds)
  }
  
  return(NA)
}

#' Extract points from race table cell
#' 
#' @param points_cell List containing points data from race table
#' @param category Character string specifying the category to extract points for
#' @return Numeric points value or NA if not found
#' @export
extract_points <- function(points_cell, category) {
  if (is.null(points_cell) || length(points_cell) == 0) return(NA)
  
  # Find the points span for the specified category
  for (span in points_cell) {
    if (grepl(category, span$class, ignore.case = TRUE)) {
      value <- span$value
      if (value == "") return(NA)
      return(as.numeric(value))
    }
  }
  
  return(NA)
}

#' Validate race table structure
#' 
#' @param table List containing race table data
#' @return Logical indicating if table structure is valid
#' @export
validate_race_table <- function(table) {
  required_fields <- c("rank", "bib", "name", "club", "overall_time")
  
  # Check if all required fields are present
  for (field in required_fields) {
    if (!field %in% names(table)) {
      warning(sprintf("Missing required field: %s", field))
      return(FALSE)
    }
  }
  
  # Check if points field exists
  if (!"points" %in% names(table)) {
    warning("Missing points field")
    return(FALSE)
  }
  
  # Check if all vectors have the same length
  lengths <- sapply(table, length)
  if (length(unique(lengths)) > 1) {
    warning("Inconsistent number of rows in table")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Process race table data
#' 
#' @param table List containing raw race table data
#' @return List containing processed race table data
#' @export
process_race_table <- function(table) {
  if (!validate_race_table(table)) {
    stop("Invalid race table structure")
  }
  
  # Clean times
  table$overall_time <- sapply(table$overall_time, clean_race_time)
  
  # Extract points for each category
  categories <- unique(sapply(table$points[[1]], function(x) {
    gsub("points-\\d+-", "", x$class)
  }))
  
  for (category in categories) {
    table[[paste0("points_", category)]] <- sapply(table$points, function(x) {
      extract_points(x, category)
    })
  }
  
  return(table)
} 