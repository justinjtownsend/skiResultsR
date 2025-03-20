# Load required packages
library(xml2)
library(rvest)
library(magrittr)

# Source the extract_tables function
source("R/extract_tables.R")

# Specify the path to your sample file
file_path <- "inst/extdata/chatham_oct2023.html"

# Extract tables
tables <- extract_raceTables(file_path)

# Print summary of results
cat("Number of race tables found:", length(tables), "\n\n")

# Print information about each race table
cat("Race tables found:\n\n")
for (race_id in names(tables)) {
  cat("Race", race_id, ":\n")
  cat("Columns:", paste(names(tables[[race_id]]), collapse = ", "), "\n")
  cat("Number of rows:", nrow(tables[[race_id]]), "\n\n")
}

# Examine first race table in detail
cat("Examining first race table (race-", names(tables)[1], "):\n\n")
first_table <- tables[[1]]

# Print first 3 rows in a readable format
cat("First 3 rows:\n\n")
for (row_idx in 1:min(3, nrow(first_table))) {
  cat("Row", row_idx, ":\n")
  for (col_name in names(first_table)) {
    cell_data <- first_table[[col_name]][[row_idx]]
    cat(col_name, ":", cell_data$main, "\n")
    
    # Print links if they exist
    if (length(cell_data$links) > 0) {
      cat("  Links:\n")
      for (link in cell_data$links) {
        cat("   ", link$text, "->", link$href, "\n")
      }
    }
    
    # Print points if they exist
    if (length(cell_data$points) > 0) {
      cat("  Points:\n")
      for (point in cell_data$points) {
        cat("   ", point$class, ":", point$value, "\n")
      }
    }
  }
  cat("\n")
}

# Print sample points structure for first cell with points
cat("Sample points structure for first cell with points:\n")
first_cell_with_points <- NULL
for (col_name in names(first_table)) {
  if (length(first_table[[col_name]][[1]]$points) > 0) {
    first_cell_with_points <- first_table[[col_name]][[1]]
    cat("Row 1, Column", col_name, "\n")
    break
  }
}

if (!is.null(first_cell_with_points)) {
  cat("Number of point spans:", length(first_cell_with_points$points), "\n")
  cat("Points spans:\n")
  for (point in first_cell_with_points$points) {
    cat("  Class:", point$class, "\n")
    cat("  Style:", point$style, "\n")
    cat("  Value:", point$value, "\n\n")
  }
} 