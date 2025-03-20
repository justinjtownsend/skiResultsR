#' Extract race tables from an HTML file
#'
#' This function extracts race tables from an HTML file containing ski race results.
#' It specifically looks for tables with IDs starting with "race-" and preserves
#' the nested structure of the data, including points and links.
#'
#' @param file_path Path to the HTML file containing race results
#' @return A list of data frames, each containing a race table with the following structure:
#'   - Each cell contains a list with:
#'     - main: The main text content
#'     - points: A list of point spans with class, style, and value (if any exist)
#'     - links: A list of links with text and href (if any exist)
#'   - Tables are named by their race ID (e.g., "race-9973")
#' @export
extract_raceTables <- function(file_path) {
  # Read the HTML file
  html_content <- xml2::read_html(file_path)
  
  # Find all tables with IDs starting with "race-"
  tables <- rvest::html_elements(html_content, "table[id^='race-']")
  
  # Initialize a list to store results
  results <- list()
  
  # Process each table
  for (table in tables) {
    # Get the race ID from the table
    race_id <- rvest::html_attr(table, "id")
    
    # Get all rows
    rows <- rvest::html_elements(table, "tr")
    
    # Process header row to get column names
    header_row <- rows[[1]]
    headers <- rvest::html_elements(header_row, "th")
    column_names <- sapply(headers, function(h) rvest::html_text(h))
    
    # Create a matrix to store cell data
    num_rows <- length(rows) - 1  # Subtract 1 for header row
    num_cols <- length(column_names)
    cell_matrix <- matrix(list(), nrow = num_rows, ncol = num_cols)
    
    # Process data rows (skip header row)
    for (row_idx in 2:length(rows)) {
      row <- rows[[row_idx]]
      cells <- rvest::html_elements(row, "td")
      
      # Process each cell in the row
      for (col_idx in 1:num_cols) {
        # Get cell if it exists, otherwise create empty cell
        cell <- if (col_idx <= length(cells)) cells[[col_idx]] else NULL
        
        if (!is.null(cell)) {
          # Extract main text content
          main_text <- rvest::html_text(cell)
          
          # Extract points spans if they exist
          points_spans <- rvest::html_elements(cell, "span[class^='points-']")
          points_data <- if (length(points_spans) > 0) {
            lapply(points_spans, function(span) {
              list(
                class = rvest::html_attr(span, "class"),
                style = rvest::html_attr(span, "style"),
                value = rvest::html_text(span)
              )
            })
          } else {
            list()  # Empty list if no points spans found
          }
          
          # Extract links if they exist
          links <- rvest::html_elements(cell, "a")
          links_data <- if (length(links) > 0) {
            lapply(links, function(link) {
              list(
                text = rvest::html_text(link),
                href = rvest::html_attr(link, "href")
              )
            })
          } else {
            list()  # Empty list if no links found
          }
        } else {
          # Create empty cell data
          main_text <- ""
          points_data <- list()
          links_data <- list()
        }
        
        # Store cell data in matrix
        cell_matrix[row_idx - 1, col_idx] <- list(list(
          main = main_text,
          points = points_data,
          links = links_data
        ))
      }
    }
    
    # Convert matrix to data frame
    df <- as.data.frame(cell_matrix)
    colnames(df) <- column_names
    
    # Add to results
    results[[race_id]] <- df
  }
  
  return(results)
}

#' Process all HTML files in the package's extdata directory
#'
#' @return List of lists containing tables from each HTML file
#' @export
process_raceEvent_files <- function() {
  # Get list of HTML files in extdata
  extdata_dir <- system.file("extdata", package = "skiResultsR")
  html_files <- list.files(extdata_dir, pattern = "\\.html$", full.names = TRUE)
  
  # Process each file
  result <- lapply(html_files, function(file) {
    list(
      file = basename(file),
      tables = extract_raceTables(file)
    )
  })
  
  result
} 