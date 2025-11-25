#' Extract a specific race table by race ID from an event HTML file
#'
#' This is the main (workhorse) function for the package. It produces understandable
#' race results for any race by pre-processing HTML tables and then using
#' rvest::html_table() to produce the final data frame.
#'
#' @param file_path Path to the HTML file containing event results
#' @param race_id Race identifier in the format 'race-9973'
#' @return A data frame containing the race results
#' @family race functions
#' @seealso [get_races()] for extracting all races from an event,
#'   [get_race_types()] for getting available race IDs
#' @export
#' @examples
#' \dontrun{
#' # Extract specific race by ID
#' file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
#' race_data <- get_race(file_path, "race-9973")
#' 
#' head(race_data)
#' }
get_race <- function(file_path, race_id) {
  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  if (missing(race_id) || is.null(race_id) || race_id == "") {
    stop("race_id is required (e.g., 'race-9973')")
  }
  
  # Read HTML file
  html_content <- xml2::read_html(file_path)
  
  # Find the table with the specified race_id using XPath
  xpath_expr <- paste0("//table[@id='", race_id, "']")
  race_table <- html_content %>%
    rvest::html_element(xpath = xpath_expr)
  
  if (is.null(race_table) || length(race_table) == 0) {
    stop("Race table not found for race_id '", race_id, "' in file: ", file_path)
  }
  
  # Pre-process the table according to the rules
  race_table <- .preprocess_race_table(race_table)
  
  # Use rvest::html_table() to produce the final data frame
  race_data <- race_table %>%
    rvest::html_table(fill = TRUE)
  
  # Rule 9: Drop empty trailing columns (from ignored Points data)
  race_data <- .drop_empty_trailing_columns(race_data)
  
  return(race_data)
}

#' Internal function to pre-process race table HTML
#' 
#' Applies the pre-processing rules for headers and body rows before
#' using rvest::html_table()
#' 
#' @param table HTML table element
#' @return Pre-processed HTML table element
#' @keywords internal
.preprocess_race_table <- function(table) {
  # Process headers (rules 5.1, 5.2, 5.3)
  table <- .process_table_headers(table)
  
  # Process body (rules 6.1, 6.2, 6.3, 6.4)
  table <- .process_table_body(table)
  
  return(table)
}

#' Internal function to process table headers
#' 
#' Rule 5.1: If header columns contain nesting, choose the element's inner text
#' Rule 5.2: If header column contains nesting and inner text includes "Points",
#'           ignore this element and all child elements from further processing
#' Rule 5.3: If header column element's inner text is blank (null), return "Win For"
#' Rule 5.4: If the header contains elements where the inner text is the same,
#'           add a sequence number to the end (e.g. Bib.1, Bib.2, Competitor.1, Competitor.2)
#' 
#' @param table HTML table element
#' @return Table with processed headers
#' @keywords internal
.process_table_headers <- function(table) {
  # Get all header cells
  header_cells <- table %>%
    rvest::html_elements("thead tr th")
  
  if (length(header_cells) == 0) {
    # Try alternative header location
    header_cells <- table %>%
      rvest::html_elements("tr:first-child th")
  }
  
  # First pass: process rules 5.1, 5.2, 5.3 and collect all header texts
  header_texts <- character(length(header_cells))
  
  for (i in seq_along(header_cells)) {
    th <- header_cells[[i]]
    
    # Get the inner text
    inner_text <- rvest::html_text2(th)
    
    # Rule 5.3: If blank, set to "Win For"
    if (is.null(inner_text) || trimws(inner_text) == "") {
      xml2::xml_text(th) <- "Win For"
      header_texts[i] <- "Win For"
      next
    }
    
    # Rule 5.2: Check if inner text includes "Points"
    # If so, ignore this element and all child elements from further processing
    if (grepl("Points", inner_text, ignore.case = TRUE)) {
      # Remove all child elements and set text to empty
      xml2::xml_remove(xml2::xml_children(th), free = TRUE)
      xml2::xml_text(th) <- ""
      header_texts[i] <- ""
      next
    }
    
    # Store the text for duplicate detection (Rule 5.4)
    header_texts[i] <- trimws(inner_text)
    # Note: Rule 5.1 is handled automatically by html_text2() which extracts
    # text from nested elements correctly
  }
  
  # Second pass: Rule 5.4 - Handle duplicate header text by adding sequence numbers
  # This works for ANY duplicate header text in ANY positions (e.g., Bib, Competitor, etc.)
  # The column positions in the examples are just examples - this finds duplicates anywhere
  
  # Track which headers have been processed to avoid re-processing
  processed <- logical(length(header_cells))
  
  # Process each header cell
  for (i in seq_along(header_cells)) {
    # Skip if already processed or empty
    if (processed[i] || header_texts[i] == "" || is.null(header_texts[i])) {
      next
    }
    
    current_text <- header_texts[i]
    
    # Find all indices with the same text (works for any positions in the header)
    duplicate_indices <- which(header_texts == current_text & !processed)
    
    # If there are duplicates (more than one occurrence of this text)
    if (length(duplicate_indices) > 1) {
      # Process all duplicates together, adding sequence numbers based on their order
      for (dup_idx in seq_along(duplicate_indices)) {
        dup_position <- duplicate_indices[dup_idx]
        
        # Add sequence number to the end (e.g., Bib.1, Bib.2, Competitor.1, Competitor.2)
        new_text <- paste0(current_text, ".", dup_idx)
        
        # Update the header cell
        th <- header_cells[[dup_position]]
        xml2::xml_text(th) <- new_text
        
        # Mark as processed
        processed[dup_position] <- TRUE
      }
    } else {
      # No duplicates, mark as processed
      processed[i] <- TRUE
    }
  }
  
  return(table)
}

#' Internal function to process table body
#' 
#' Rule 6.1: If body rows contain nesting, choose the element's inner text
#' Rule 6.2: If body row contains nesting and element's inner text is blank
#'           and children have the 'points-' class, ignore this element and all child elements
#' Rule 6.3: If body rows contain attribute 'class=win_for_{n}', capture the element attribute text
#' Rule 6.4: Insert captured element attribute text into the empty element
#' 
#' @param table HTML table element
#' @return Table with processed body
#' @keywords internal
.process_table_body <- function(table) {
  # Get all body rows
  body_rows <- table %>%
    rvest::html_elements("tbody tr")
  
  if (length(body_rows) == 0) {
    # Try alternative body location (rows that are not headers)
    all_rows <- table %>%
      rvest::html_elements("tr")
    if (length(all_rows) > 1) {
      body_rows <- all_rows[-1]  # Skip first row (header)
    }
  }
  
  for (row_idx in seq_along(body_rows)) {
    tr <- body_rows[[row_idx]]
    
    # Rule 6.3: Check for win_for_{n} class attribute
    tr_class <- xml2::xml_attr(tr, "class")
    win_for_value <- NULL
    
    if (!is.null(tr_class) && grepl("win_for_", tr_class)) {
      # Extract the number from win_for_{n}
      win_match <- regmatches(tr_class, regexpr("win_for_\\d+", tr_class))
      if (length(win_match) > 0) {
        win_for_value <- win_match[1]
      }
    }
    
    # Get all cells in this row
    cells <- tr %>%
      rvest::html_elements("td")
    
    for (cell_idx in seq_along(cells)) {
      td <- cells[[cell_idx]]
      
      # Rule 6.4: If we have a win_for value and this is a details cell, insert it
      if (!is.null(win_for_value)) {
        td_class <- xml2::xml_attr(td, "class")
        if (!is.null(td_class) && grepl("details", td_class)) {
          xml2::xml_text(td) <- win_for_value
          next
        }
      }
      
      # Rule 6.2: Check for 'points-' class FIRST (before extracting any text)
      # If found and element's inner text is blank, ignore this element and all child elements
      # DO NOT RETRIEVE THE INNER TEXT OF ANY CHILDREN!
      # Find all spans with points- class using XPath to ensure we get all nested ones
      points_spans <- xml2::xml_find_all(td, ".//span[contains(@class, 'points-')]")
      
      if (length(points_spans) > 0) {
        # Remove all points- spans from the DOM to prevent text extraction
        # This ensures html_table() won't extract their text
        # Remove in reverse order to avoid index issues
        for (i in length(points_spans):1) {
          xml2::xml_remove(points_spans[[i]], free = TRUE)
        }
        
        # Now check if the element's inner text (after removing points- spans) is blank
        remaining_text <- rvest::html_text2(td)
        if (is.null(remaining_text) || trimws(remaining_text) == "") {
          # Element is blank after removing points- spans, so ignore it completely
          xml2::xml_remove(xml2::xml_children(td), free = TRUE)
          xml2::xml_text(td) <- ""
          next
        }
        # If there's remaining text, continue processing normally (points- already removed)
      }
      
      # Rule 6.1: If body rows contain nesting, choose the element's inner text
      # This is handled automatically by html_text2() which extracts text
      # from nested elements (like links) correctly, so no special processing needed
    }
  }
  
  return(table)
}

#' Internal function to drop empty trailing columns
#' 
#' Rule 9: Drop empty trailing columns (from ignored Points data)
#' Removes columns from the end of the data frame that are completely empty
#' 
#' @param df Data frame
#' @return Data frame with empty trailing columns removed
#' @keywords internal
.drop_empty_trailing_columns <- function(df) {
  if (ncol(df) == 0) {
    return(df)
  }
  
  # Check columns from the end, removing empty ones
  # A column is considered empty if all values are NA, empty strings, or whitespace
  for (col_idx in ncol(df):1) {
    col_data <- df[[col_idx]]
    
    # Convert to character for checking (handles NA, NULL, etc.)
    col_char <- as.character(col_data)
    col_char[is.na(col_char)] <- ""
    
    # Check if column is empty (all NA, empty strings, or whitespace)
    is_empty <- all(trimws(col_char) == "")
    
    if (is_empty) {
      # Remove this column
      df <- df[, -col_idx, drop = FALSE]
    } else {
      # Found a non-empty column, stop removing
      break
    }
  }
  
  return(df)
}

# ===== get_races functions =====

#' Extract all races from an event HTML file
#'
#' This function extracts all race results from a skiresults.co.uk event HTML file.
#' It uses get_race_types() to get the list of race IDs, then calls get_race()
#' for each race ID to extract the complete race results.
#'
#' @param file_path Path to the HTML file containing event results
#' @return A named list of data frames, where each element is named by race_id
#'   and contains the race results from get_race()
#' @family race functions
#' @seealso [get_race()] for extracting a single race,
#'   [get_race_types()] for getting available race IDs
#' @export
#' @examples
#' \dontrun{
#' # Extract all races from an event
#' file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
#' races <- get_races(file_path)
#' 
#' # Access specific race by ID
#' names(races)
#' races[["race-9973"]]
#' }
get_races <- function(file_path) {
  # Validate input
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Step 3: Extract race_id column from the tibble returned by get_race_types()
  race_types_df <- get_race_types(file_path)
  
  # Extract race_id column
  race_ids <- race_types_df$race_id
  
  # Filter out NA race_ids
  race_ids <- race_ids[!is.na(race_ids) & race_ids != ""]
  
  if (length(race_ids) == 0) {
    warning("No valid race IDs found in file: ", file_path)
    return(list())
  }
  
  # Step 4: Using lapply(), each race_id is passed to get_race()
  races <- lapply(race_ids, function(race_id) {
    tryCatch({
      get_race(file_path, race_id = race_id)
    }, error = function(e) {
      warning("Failed to extract race '", race_id, "': ", e$message)
      return(NULL)
    })
  })
  
  # Filter out NULL results
  races <- races[!sapply(races, is.null)]
  
  # Step 5: race_id is used as the list name, for each result returned by get_race()
  names(races) <- race_ids[seq_along(races)]
  
  return(races)
}

#' Internal function to extract race metadata
#' @param html_content Parsed HTML content
#' @param race_id Race identifier
#' @return List with race metadata
#' @keywords internal
.extract_race_metadata <- function(html_content, race_id) {
  # Look for race-specific information near the table
  # This could include race name, category, etc.
  
  # Find the table element
  table_element <- html_content %>%
    rvest::html_element(paste0("#", race_id))
  
  metadata <- list(
    race_id = race_id,
    table_classes = NULL,
    display_style = NULL
  )
  
  if (!is.null(table_element)) {
    # Extract table attributes
    metadata$table_classes <- rvest::html_attr(table_element, "class")
    metadata$display_style <- rvest::html_attr(table_element, "style")
    
    # Look for preceding headings or labels
    # Try to find the nearest heading before this table
    preceding_elements <- html_content %>%
      rvest::html_elements("h1, h2, h3, h4, h5, h6")
    
    # This is a simplified approach - in practice, you might need more
    # sophisticated logic to associate headings with tables
    if (length(preceding_elements) > 0) {
      last_heading <- preceding_elements[length(preceding_elements)] %>%
        rvest::html_text() %>%
        trimws()
      metadata$section_heading <- last_heading
    }
  }
  
  return(metadata)
}

#' Internal function to extract all race tables
#' @param html_content Parsed HTML content
#' @return List of race data frames
#' @keywords internal
.extract_all_races <- function(html_content) {
  # Find all tables with IDs starting with "race-"
  race_tables <- html_content %>%
    rvest::html_elements("table[id^='race-']")
  
  if (length(race_tables) == 0) {
    warning("No race tables found in the HTML file")
    return(list())
  }
  
  races <- list()
  
  for (table in race_tables) {
    race_id <- rvest::html_attr(table, "id")
    race_data <- .parse_race_table(table)
    races[[race_id]] <- race_data
  }
  
  return(races)
}

#' Internal function to parse a single race table
#' @param table HTML table element
#' @return Data frame with race results
#' @keywords internal
.parse_race_table <- function(table) {
  # Get headers
  headers <- table %>%
    rvest::html_elements("thead tr th, tr:first-child th") %>%
    rvest::html_text() %>%
    trimws()
  
  if (length(headers) == 0) {
    # Fallback: get first row as headers
    first_row <- table %>%
      rvest::html_elements("tr:first-child td, tr:first-child th") %>%
      rvest::html_text() %>%
      trimws()
    headers <- first_row
  }
  
  # Get data rows (skip header row)
  data_rows <- table %>%
    rvest::html_elements("tbody tr, tr:not(:first-child)")
  
  if (length(data_rows) == 0) {
    # If no tbody, get all rows except first
    all_rows <- table %>%
      rvest::html_elements("tr")
    if (length(all_rows) > 1) {
      data_rows <- all_rows[-1]
    } else {
      return(data.frame())
    }
  }
  
  # Parse each row
  race_data <- list()
  
  for (i in seq_along(data_rows)) {
    row <- data_rows[[i]]
    cells <- row %>%
      rvest::html_elements("td, th")
    
    row_data <- list()
    
    for (j in seq_along(cells)) {
      cell <- cells[[j]]
      
      # Extract main text
      main_text <- cell %>%
        rvest::html_text() %>%
        trimws()
      
      # Extract links if present
      links <- cell %>%
        rvest::html_elements("a") %>%
        purrr::map(~ list(
          text = rvest::html_text(.x),
          href = rvest::html_attr(.x, "href")
        ))
      
      # Extract points spans if present
      points <- cell %>%
        rvest::html_elements("span[class^='points-']") %>%
        purrr::map(~ list(
          class = rvest::html_attr(.x, "class"),
          style = rvest::html_attr(.x, "style"),
          value = rvest::html_text(.x)
        ))
      
      # Store cell data
      if (j <= length(headers)) {
        col_name <- headers[j]
        if (col_name == "" || is.na(col_name)) {
          col_name <- paste0("col_", j)
        }
        
        row_data[[col_name]] <- list(
          text = main_text,
          links = links,
          points = points
        )
      }
    }
    
    race_data[[i]] <- row_data
  }
  
  # Convert to data frame format
  if (length(race_data) > 0 && length(headers) > 0) {
    df_data <- list()
    
    for (col in headers) {
      if (col == "" || is.na(col)) next
      
      df_data[[col]] <- sapply(race_data, function(row) {
        if (col %in% names(row)) {
          row[[col]]$text
        } else {
          NA_character_
        }
      })
      
      # Store additional data as attributes
      links_data <- lapply(race_data, function(row) {
        if (col %in% names(row)) row[[col]]$links else list()
      })
      attr(df_data[[col]], "links") <- links_data
      
      points_data <- lapply(race_data, function(row) {
        if (col %in% names(row)) row[[col]]$points else list()
      })
      attr(df_data[[col]], "points") <- points_data
    }
    
    return(as.data.frame(df_data, stringsAsFactors = FALSE))
  } else {
    return(data.frame())
  }
}

# ===== get_racers functions =====

#' Extract registered racers from a specific race
#'
#' This function extracts registered racers from a skiResults event race table.
#' Registered racers are identified by a bib number in the race table.
#'
#' @param file_path Path to the HTML file containing event results
#' @param race_id Race identifier in the format 'race-9973'
#' @return A tibble containing:
#'   \describe{
#'     \item{Rank}{Racer rank/position}
#'     \item{Bib}{Racer bib number}
#'     \item{(Rk)}{Racer rank}
#'     \item{Cat.}{Race category}
#'     \item{Name}{Racer name}
#'     \item{Profile URL}{URL to racer's profile page}
#'     \item{Club}{Racer's club}
#'   }
#' @family data extraction functions
#' @seealso [get_points()] for extracting points data,
#'   [get_clubs()] for extracting club information,
#'   [get_race()] for extracting complete race results
#' @export
#' @examples
#' \dontrun{
#' # Extract racers from a specific race
#' file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
#' racers <- get_racers(file_path, "race-9973")
#' 
#' head(racers)
#' nrow(racers)  # Number of registered racers
#' }
get_racers <- function(file_path, race_id) {
  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  if (missing(race_id) || is.null(race_id) || race_id == "") {
    stop("race_id is required (e.g., 'race-9973')")
  }
  
  # Read HTML file
  html_content <- xml2::read_html(file_path)
  
  # Step 3: Check for Bib column in header
  xpath_header_row <- paste0("//table[@id='", race_id, "']/thead/tr")
  header_row <- html_content %>%
    rvest::html_element(xpath = xpath_header_row)
  
  if (is.null(header_row)) {
    stop("Header row not found for race_id '", race_id, "'")
  }
  
  # Get all header cells and check for Bib column
  header_cells_all <- header_row %>%
    rvest::html_elements("th")
  
  bib_found <- FALSE
  for (th in header_cells_all) {
    header_text <- rvest::html_text2(th) %>% trimws()
    if (grepl("Bib", header_text, ignore.case = TRUE)) {
      bib_found <- TRUE
      break
    }
  }
  
  if (!bib_found) {
    stop("Bib column not found in table header for race_id '", race_id, 
         "'. Expected path: //table[@id='", race_id, "']/thead/tr/th[contains(text(),'Bib')]")
  }
  
  # Step 3.2: Check for Bib column in table body (verify table structure)
  xpath_bib_body_check <- paste0("//table[@id='", race_id, "']/tbody/tr/td")
  body_cells <- html_content %>%
    rvest::html_elements(xpath = xpath_bib_body_check)
  
  if (length(body_cells) == 0) {
    stop("Table body not found or empty for race_id '", race_id, "'")
  }
  
  # Step 5.1: Find column positions dynamically
  # (header_row already found in step 3)
  
  # Get all header cells
  header_cells <- header_row %>%
    rvest::html_elements("th")
  
  # Find column positions
  col_positions <- list()
  
  for (i in seq_along(header_cells)) {
    header_text <- rvest::html_text2(header_cells[[i]])
    header_text <- trimws(header_text)
    
    # Find Rank column
    if (grepl("^Rank$", header_text, ignore.case = TRUE)) {
      col_positions$Rank <- i
    }
    
    # Find Bib column
    if (grepl("Bib", header_text, ignore.case = TRUE)) {
      col_positions$Bib <- i
    }
    
    # Find (Rk) column
    if (grepl("\\(Rk\\)", header_text, ignore.case = TRUE)) {
      col_positions$`(Rk)` <- i
    }
    
    # Find Cat. column
    if (grepl("^Cat\\.$", header_text, ignore.case = TRUE)) {
      col_positions$`Cat.` <- i
    }
    
    # Find Name column
    if (grepl("^Name$", header_text, ignore.case = TRUE)) {
      col_positions$Name <- i
    }
    
    # Find Club column
    if (grepl("^Club$", header_text, ignore.case = TRUE)) {
      col_positions$Club <- i
    }
  }
  
  # Verify required columns are found
  if (is.null(col_positions$Bib)) {
    stop("Bib column position could not be determined for race_id '", race_id, "'")
  }
  
  # Step 5.2: Extract data from each row
  xpath_body_rows <- paste0("//table[@id='", race_id, "']/tbody/tr")
  body_rows <- html_content %>%
    rvest::html_elements(xpath = xpath_body_rows)
  
  if (length(body_rows) == 0) {
    # Return empty tibble with correct structure
    return(tibble::tibble(
      Rank = character(0),
      Bib = character(0),
      `(Rk)` = character(0),
      `Cat.` = character(0),
      Name = character(0),
      `Profile URL` = character(0),
      Club = character(0)
    ))
  }
  
  # Initialize result vectors
  n_rows <- length(body_rows)
  Rank <- character(n_rows)
  Bib <- character(n_rows)
  `(Rk)` <- character(n_rows)
  `Cat.` <- character(n_rows)
  Name <- character(n_rows)
  `Profile URL` <- character(n_rows)
  Club <- character(n_rows)
  
  # Extract data from each row
  for (row_idx in seq_along(body_rows)) {
    tr <- body_rows[[row_idx]]
    
    # Extract Rank
    if (!is.null(col_positions$Rank)) {
      xpath_rank <- paste0(".//td[", col_positions$Rank, "]")
      rank_cell <- tr %>%
        rvest::html_element(xpath = xpath_rank)
      if (!is.null(rank_cell)) {
        Rank[row_idx] <- rvest::html_text2(rank_cell) %>% trimws()
      }
    }
    
    # Extract Bib
    xpath_bib <- paste0(".//td[", col_positions$Bib, "]")
    bib_cell <- tr %>%
      rvest::html_element(xpath = xpath_bib)
    if (!is.null(bib_cell)) {
      Bib[row_idx] <- rvest::html_text2(bib_cell) %>% trimws()
    }
    
    # Extract (Rk)
    if (!is.null(col_positions$`(Rk)`)) {
      xpath_rk <- paste0(".//td[", col_positions$`(Rk)`, "]")
      rk_cell <- tr %>%
        rvest::html_element(xpath = xpath_rk)
      if (!is.null(rk_cell)) {
        `(Rk)`[row_idx] <- rvest::html_text2(rk_cell) %>% trimws()
      }
    }
    
    # Extract Cat.
    if (!is.null(col_positions$`Cat.`)) {
      xpath_cat <- paste0(".//td[", col_positions$`Cat.`, "]")
      cat_cell <- tr %>%
        rvest::html_element(xpath = xpath_cat)
      if (!is.null(cat_cell)) {
        `Cat.`[row_idx] <- rvest::html_text2(cat_cell) %>% trimws()
      }
    }
    
    # Extract Name
    if (!is.null(col_positions$Name)) {
      xpath_name <- paste0(".//td[", col_positions$Name, "]")
      name_cell <- tr %>%
        rvest::html_element(xpath = xpath_name)
      if (!is.null(name_cell)) {
        Name[row_idx] <- rvest::html_text2(name_cell) %>% trimws()
      }
    }
    
    # Extract Profile URL
    if (!is.null(col_positions$Name)) {
      xpath_profile <- paste0(".//td[", col_positions$Name, "]/a[contains(@href, '/people/')]")
      profile_link <- tr %>%
        rvest::html_element(xpath = xpath_profile)
      if (!is.null(profile_link)) {
        `Profile URL`[row_idx] <- rvest::html_attr(profile_link, "href")
      }
    }
    
    # Extract Club
    if (!is.null(col_positions$Club)) {
      xpath_club <- paste0(".//td[", col_positions$Club, "]")
      club_cell <- tr %>%
        rvest::html_element(xpath = xpath_club)
      if (!is.null(club_cell)) {
        Club[row_idx] <- rvest::html_text2(club_cell) %>% trimws()
      }
    }
  }
  
  # Step 6: Return tibble
  result <- tibble::tibble(
    Rank = Rank,
    Bib = Bib,
    `(Rk)` = `(Rk)`,
    `Cat.` = `Cat.`,
    Name = Name,
    `Profile URL` = `Profile URL`,
    Club = Club
  )
  
  return(result)
}

#' Internal function to find a column by possible names
#' @param col_names Vector of column names
#' @param possible_names Vector of possible names to match
#' @return Column name or NULL if not found
#' @keywords internal
.find_column <- function(col_names, possible_names) {
  col_names_lower <- tolower(col_names)
  
  for (name in possible_names) {
    matches <- which(col_names_lower == tolower(name))
    if (length(matches) > 0) {
      return(col_names[matches[1]])
    }
  }
  
  # Try partial matches
  for (name in possible_names) {
    matches <- grep(tolower(name), col_names_lower)
    if (length(matches) > 0) {
      return(col_names[matches[1]])
    }
  }
  
  return(NULL)
}

# ===== get_points functions =====

#' Extract points data for a specific race
#'
#' This function tracks the points gained by individual racers for a specific race.
#' It produces a data frame with one row per racer, with columns expanded to include
#' all points categories. Each row represents one racer with their points spread
#' across multiple columns (one per points category).
#'
#' @param file_path Path to the HTML file containing event results
#' @param race_id Race identifier in the format 'race-9973'
#' @return A data frame with one row per racer and columns for:
#'   \describe{
#'     \item{Rank}{Racer rank/position}
#'     \item{Bib}{Racer bib number}
#'     \item{(Rk)}{Racer rank}
#'     \item{Cat.}{Race category}
#'     \item{Name}{Racer name}
#'     \item{[Points Category 1]}{Points for first category}
#'     \item{[Points Category 2]}{Points for second category}
#'     \item{...}{Additional points category columns}
#'   }
#' @family data extraction functions
#' @seealso [get_racers()] for extracting racer information,
#'   [get_clubs()] for extracting club information,
#'   [get_race()] for extracting complete race results
#' @export
#' @examples
#' \dontrun{
#' # Extract points for a specific race
#' file_path <- system.file("extdata", "brentwood_jun2023.html", package = "skiresultsR")
#' points_data <- get_points(file_path, "race-9711")
#' 
#' head(points_data)
#' nrow(points_data)  # Number of racers (one row per racer)
#' ncol(points_data)  # Number of columns (5 static + dynamic points columns)
#' }
get_points <- function(file_path, race_id) {
  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  if (missing(race_id) || is.null(race_id) || race_id == "") {
    stop("race_id is required (e.g., 'race-9973')")
  }
  
  # Read HTML file
  html_content <- xml2::read_html(file_path)
  
  # Step 3: Check if points data exists
  xpath_points_check <- paste0("//table[@id='", race_id, "']/thead/tr/th/ul/li[contains(@data-id, 'points-')]")
  points_header_elements <- html_content %>%
    rvest::html_elements(xpath = xpath_points_check)
  
  if (length(points_header_elements) == 0) {
    stop("Points data not found for race_id '", race_id, 
         "'. Expected path: //table[@id='", race_id, "']/thead/tr/th/ul/li[contains(@data-id, 'points-')]")
  }
  
  # Find the table
  xpath_table <- paste0("//table[@id='", race_id, "']")
  race_table <- html_content %>%
    rvest::html_element(xpath = xpath_table)
  
  if (is.null(race_table) || length(race_table) == 0) {
    stop("Race table not found for race_id '", race_id, "' in file: ", file_path)
  }
  
  # Step 4: Extract columns
  # Step 4.1: Static columns from header
  static_cols <- c("Rank", "Bib", "(Rk)", "Cat.", "Name")
  
  # Step 4.2: Dynamic columns from ul/li elements
  xpath_dynamic_cols <- paste0("//table[@id='", race_id, "']/thead/tr/th/ul/li[contains(@data-id, 'points-')]")
  dynamic_col_elements <- html_content %>%
    rvest::html_elements(xpath = xpath_dynamic_cols)
  
  # Extract inner text for each dynamic column
  dynamic_col_names <- character(length(dynamic_col_elements))
  for (i in seq_along(dynamic_col_elements)) {
    dynamic_col_names[i] <- rvest::html_text2(dynamic_col_elements[[i]])
  }
  
  # All column names
  all_col_names <- c(static_cols, dynamic_col_names)
  
  # Step 5: Extract rows
  # Get all body rows
  xpath_body_rows <- paste0("//table[@id='", race_id, "']/tbody/tr")
  body_rows <- html_content %>%
    rvest::html_elements(xpath = xpath_body_rows)
  
  if (length(body_rows) == 0) {
    # Try alternative location
    xpath_body_rows_alt <- paste0("//table[@id='", race_id, "']/tr[position()>1]")
    body_rows <- html_content %>%
      rvest::html_elements(xpath = xpath_body_rows_alt)
  }
  
  if (length(body_rows) == 0) {
    warning("No body rows found for race_id '", race_id, "'")
    # Return empty data frame with correct column structure
    empty_df <- data.frame(matrix(ncol = length(all_col_names), nrow = 0))
    names(empty_df) <- all_col_names
    return(empty_df)
  }
  
  # Find the Points column position dynamically
  # Different races have different numbers of columns before Points
  # Look for the td that contains span[contains(@class, 'points-')] in the first row
  points_col_position <- NULL
  
  if (length(body_rows) > 0) {
    first_row <- body_rows[[1]]
    first_row_cells <- first_row %>%
      rvest::html_elements("td")
    
    # Find which td contains points spans (using XPath)
    for (cell_idx in seq_along(first_row_cells)) {
      cell <- first_row_cells[[cell_idx]]
      points_spans_in_cell <- cell %>%
        rvest::html_elements(xpath = ".//span[contains(@class, 'points-')]")
      
      if (length(points_spans_in_cell) > 0) {
        points_col_position <- cell_idx
        break
      }
    }
  }
  
  # Fallback: if not found in body, try to find by header
  if (is.null(points_col_position)) {
    # Find the header th that contains the points ul/li
    header_row <- html_content %>%
      rvest::html_element(xpath = paste0("//table[@id='", race_id, "']/thead/tr"))
    
    if (!is.null(header_row)) {
      header_cells <- header_row %>%
        rvest::html_elements("th")
      
      for (cell_idx in seq_along(header_cells)) {
        cell <- header_cells[[cell_idx]]
        points_li_in_cell <- cell %>%
          rvest::html_elements(xpath = ".//ul/li[contains(@data-id, 'points-')]")
        
        if (length(points_li_in_cell) > 0) {
          points_col_position <- cell_idx
          break
        }
      }
    }
  }
  
  # Final fallback to td[11] if still not found (for backward compatibility)
  if (is.null(points_col_position)) {
    points_col_position <- 11
  }
  
  # Pre-allocate data frame (hint from step 8)
  n_rows <- length(body_rows)
  n_cols <- length(all_col_names)
  result_df <- data.frame(matrix(NA_character_, nrow = n_rows, ncol = n_cols))
  names(result_df) <- all_col_names
  
  # Step 5.1: Extract static row data
  # Step 5.2: Extract dynamic points data
  for (row_idx in seq_along(body_rows)) {
    tr <- body_rows[[row_idx]]
    
    # Extract static columns (td[1] through td[5])
    for (col_idx in 1:5) {
      xpath_cell <- paste0(".//td[", col_idx, "]")
      cell <- tr %>%
        rvest::html_element(xpath = xpath_cell)
      
      if (!is.null(cell)) {
        cell_text <- rvest::html_text2(cell)
        result_df[row_idx, col_idx] <- ifelse(is.null(cell_text) || trimws(cell_text) == "", 
                                              NA_character_, trimws(cell_text))
      }
    }
    
    # Extract dynamic points data from the dynamically found points column
    # Step 5.2: Match spans to columns by comparing header data-id with span class
    # Header has: data-id="points-9711-lsersa-fastest-female-None"
    # Body has: class="points-9711-lsersa-fastest-female-None"
    xpath_points_cell <- paste0(".//td[", points_col_position, "]")
    points_cell <- tr %>%
      rvest::html_element(xpath = xpath_points_cell)
    
    if (!is.null(points_cell)) {
      # Find all span elements with points- class
      xpath_points_spans <- ".//span[contains(@class, 'points-')]"
      points_spans <- points_cell %>%
        rvest::html_elements(xpath = xpath_points_spans)
      
      # Step 7.4: Match each span to its corresponding column by comparing
      # the span's class attribute with the header's data-id attribute
      for (span_idx in seq_along(points_spans)) {
        span <- points_spans[[span_idx]]
        span_class <- rvest::html_attr(span, "class")
        
        # Find matching column by comparing span class with header data-id
        for (col_idx in seq_along(dynamic_col_elements)) {
          header_li <- dynamic_col_elements[[col_idx]]
          header_data_id <- rvest::html_attr(header_li, "data-id")
          
          # Match: span class should contain the data-id value
          # e.g., class="points-9711-lsersa-fastest-female-None" matches data-id="points-9711-lsersa-fastest-female-None"
          if (!is.null(header_data_id) && !is.null(span_class)) {
            # Check if span class contains the data-id (the class may have additional classes)
            if (grepl(header_data_id, span_class, fixed = TRUE)) {
              # Extract inner text from span (regardless of visibility - display:none is just for UI)
              span_text <- rvest::html_text2(span)
              
              # Step 7.4: Extract value if present, otherwise preserve null
              # Don't check visibility - extract all values regardless of display style
              if (!is.null(span_text) && trimws(span_text) != "") {
                result_df[row_idx, 5 + col_idx] <- trimws(span_text)
              } else {
                # Preserve null values (step 5.2 and 7.4)
                result_df[row_idx, 5 + col_idx] <- NA_character_
              }
              break  # Found match, move to next span
            }
          }
        }
      }
      
      # Initialize all dynamic columns to NA if no spans found
      # (This ensures null values are preserved for columns without matching spans)
      if (length(points_spans) == 0) {
        for (col_idx in seq_along(dynamic_col_names)) {
          result_df[row_idx, 5 + col_idx] <- NA_character_
        }
      }
    } else {
      # No points cell found, set all dynamic columns to NA
      for (col_idx in seq_along(dynamic_col_names)) {
        result_df[row_idx, 5 + col_idx] <- NA_character_
      }
    }
  }
  
  # Step 9: Return data frame
  return(result_df)
}
