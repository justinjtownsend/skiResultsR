# Helper Functions
# Helper functions are stored in a single file helpers.R

#' Extract race types and event information from an HTML file
#'
#' This function extracts key information for races at a skiResults event.
#' It identifies all available race types for an event and returns structured
#' data with event ID and race information.
#'
#' @param file_path Path to the HTML file containing event results
#' @param ext Optional boolean parameter (Y/N) to enable detailed processing
#'   of race table header columns. If Y, adds a "cols" column with comma-separated
#'   column names for each race.
#' @return A tibble with columns:
#'       \itemize{
#'         \item{event_id}{The event ID (numeric)}
#'         \item{race_type}{The race type name (e.g., "Individual", "Club Performances")}
#'         \item{race_id}{The race ID (e.g., "race-9973")}
#'         \item{race_pts}{Indicator if race has points columns ('Y' if present, NA if not)}
#'       \item{tbl_cols}{Column names from race table header (comma-separated, only if ext=Y)}
#'       \item{tbl_cols_cnt}{Number of columns in race table header (only if ext=Y)}
#'     }
#' @family race functions
#' @seealso [get_race()] for extracting a single race by ID,
#'   [get_races()] for extracting all races from an event
#' @export
#' @examples
#' \dontrun{
#' # Extract race types from an event
#' file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
#' race_types <- get_race_types(file_path)
#' 
#' # Access race types
#' race_types
#' 
#' # Extract with column information
#' race_types_ext <- get_race_types(file_path, ext = "Y")
#' }
get_race_types <- function(file_path, ext = NULL) {
  # Validate input
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Read HTML file
  html_content <- rvest::read_html(file_path)
  
  # Find the tabs unordered list
  # Using CSS selector for ul with class "tabs"
  tabs_ul <- rvest::html_elements(html_content, "ul.tabs")
  
  if (length(tabs_ul) == 0) {
    stop("Could not find tabs section in HTML file")
  }
  
  # Get the first tabs list
  tabs_ul_first <- tabs_ul[[1]]
  
  # Extract event_id from the first <li> element's <a> tag href attribute
  # XPath: //ul[contains(@class, 'tabs')]/li[1]
  all_li <- rvest::html_elements(tabs_ul_first, "li")
  
  if (length(all_li) == 0) {
    stop("Could not find list items in tabs section")
  }
  
  # Get hrefs from all <a> tags in <li> elements, find first valid one
  all_links <- rvest::html_elements(all_li, "a")
  all_hrefs <- rvest::html_attr(all_links, "href")
  valid_hrefs <- all_hrefs[!is.na(all_hrefs) & all_hrefs != ""]
  
  if (length(valid_hrefs) == 0) {
    stop("Could not find valid event link with href in tabs section")
  }
  
  href <- valid_hrefs[1]
  
  # Extract event_id using regex: /\d+# (keep numbers only)
  event_id_match <- regmatches(href, regexpr("/\\d+#", href))
  if (length(event_id_match) == 0) {
    stop("Could not extract event ID from href: ", href)
  }
  
  # Extract just the numbers
  event_id <- gsub("/|#", "", event_id_match[1])
  event_id <- as.character(event_id)  # Keep as character for list naming
  
  # Extract race types from <li> elements
  li_elements <- rvest::html_elements(tabs_ul_first, "li")
  
  if (length(li_elements) == 0) {
    stop("Could not find race type list items in tabs section")
  }
  
  # Get all race tables in the HTML as fallback for missing hrefs
  race_tables <- html_content %>%
    rvest::html_elements("table[id^='race-']")
  all_race_ids <- rvest::html_attr(race_tables, "id")
  all_race_ids <- all_race_ids[!is.na(all_race_ids) & all_race_ids != ""]
  
  # Extract race_type (text from <a> tag) and race_id (href attribute, strip '#')
  race_data <- lapply(seq_along(li_elements), function(i) {
    li <- li_elements[[i]]
    a_tag <- rvest::html_element(li, "a")
    if (length(a_tag) == 0 || is.na(a_tag)) {
      return(NULL)
    }
    
    race_type <- trimws(rvest::html_text(a_tag))
    race_id_full <- rvest::html_attr(a_tag, "href")
    
    # Extract race_id (part after #)
    race_id <- NA_character_
    if (!is.na(race_id_full) && race_id_full != "") {
      # Extract the part after # (e.g., "race-9973" from "https://skiresults.co.uk/events/1319#race-9973")
      race_id_match <- regmatches(race_id_full, regexpr("#[^#]+$", race_id_full))
      if (length(race_id_match) > 0) {
        race_id <- gsub("^#", "", race_id_match[1])
      }
    }
    
    # Fallback: If href is empty/NA, try to match by position with race tables
    # This handles cases where the first link doesn't have an href (e.g., active tab)
    if (is.na(race_id) || race_id == "") {
      # If this is the first link and we have race tables, use the first one
      if (i == 1 && length(all_race_ids) > 0) {
        # Try to find a race table that matches the expected pattern
        # The first race is usually "Individual" and often the first table
        race_id <- all_race_ids[1]
      }
    }
    
    # Return NULL if race_type is empty
    if (is.na(race_type) || race_type == "") {
      return(NULL)
    }
    
    # Check if race has points columns
    # Step 5.2: Check for points columns using race_id
    race_pts <- NA_character_
    if (!is.na(race_id) && race_id != "") {
      # XPath: //table[@id="race-9711"]/thead/tr/th/ul/li[contains(@data-id, "points-")]
      xpath_points_check <- paste0("//table[@id='", race_id, "']/thead/tr/th/ul/li[contains(@data-id, 'points-')]")
      points_elements <- rvest::html_elements(html_content, xpath = xpath_points_check)
      
      if (length(points_elements) > 0) {
        race_pts <- "Y"
      }
    }
    
    list(
      event_id = event_id,
      race_type = race_type,
      race_id = race_id,
      race_pts = race_pts
    )
  })
  
  # Filter out NULL entries
  race_data <- race_data[!sapply(race_data, is.null)]
  
  if (length(race_data) == 0) {
    stop("Could not extract any race types from tabs section")
  }
  
  # Convert to data frame
  race_types_df <- data.frame(
    event_id = sapply(race_data, function(x) x$event_id),
    race_type = sapply(race_data, function(x) x$race_type),
    race_id = sapply(race_data, function(x) x$race_id),
    race_pts = sapply(race_data, function(x) x$race_pts),
    stringsAsFactors = FALSE
  )
  
  # Step 7: If ext = Y, add column information
  if (!is.null(ext) && (ext == "Y" || ext == "y" || ext == TRUE)) {
    # Step 7.1: Add tbl_cols column
    race_types_df$tbl_cols <- NA_character_
    # Step 7.4: Add tbl_cols_cnt column
    race_types_df$tbl_cols_cnt <- NA_integer_
    
    # Step 7.2: For each race_id, extract columns from table header
    for (i in seq_len(nrow(race_types_df))) {
      race_id <- race_types_df$race_id[i]
      
      if (!is.na(race_id) && race_id != "") {
        # Extract columns from //table[@id="{race_id}"]/thead/tr
        xpath_header <- paste0("//table[@id='", race_id, "']/thead/tr")
        header_row <- html_content %>%
          rvest::html_element(xpath = xpath_header)
        
        if (!is.null(header_row)) {
          # Get all header cells
          header_cells <- header_row %>%
            rvest::html_elements("th")
          
          # Step 7.4: Count the number of columns
          race_types_df$tbl_cols_cnt[i] <- length(header_cells)
          
          # Extract column names
          col_names <- character(length(header_cells))
          for (j in seq_along(header_cells)) {
            col_text <- rvest::html_text2(header_cells[[j]]) %>% trimws()
            col_names[j] <- ifelse(is.null(col_text) || col_text == "", "", col_text)
          }
          
          # Remove empty column names and create comma-separated string
          col_names <- col_names[col_names != ""]
          race_types_df$tbl_cols[i] <- paste(col_names, collapse = ", ")
        }
      }
    }
  }
  
  # Convert to tibble
  race_types_df <- tibble::as_tibble(race_types_df)
  
  # Step 8: Return the tibble
  return(race_types_df)
}

#' Extract clubs from a specific race
#'
#' This function extracts the clubs of registered racers from a skiResults event race table.
#' Clubs are represented by racers, so the same table identification as get_racers() is used.
#'
#' @param file_path Path to the HTML file containing event results
#' @param race_id Race identifier in the format 'race-9973'
#' @return A tibble containing:
#'   \describe{
#'     \item{Club}{Club name}
#'     \item{Profile URL}{URL to club's profile page}
#'   }
#' @family data extraction functions
#' @seealso [get_racers()] for extracting racer information,
#'   [get_points()] for extracting points data,
#'   [get_race()] for extracting complete race results
#' @export
#' @examples
#' \dontrun{
#' # Extract clubs from a specific race
#' file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
#' clubs <- get_clubs(file_path, "race-9973")
#' 
#' head(clubs)
#' nrow(clubs)  # Number of unique clubs
#' }
get_clubs <- function(file_path, race_id) {
  # Validate inputs
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  if (missing(race_id) || is.null(race_id) || race_id == "") {
    stop("race_id is required (e.g., 'race-9973')")
  }
  
  # Read HTML file
  html_content <- xml2::read_html(file_path)
  
  # Step 3: Check for Bib column in header (same as get_racers())
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
  # Get all header cells
  header_cells <- header_row %>%
    rvest::html_elements("th")
  
  # Find column positions
  col_positions <- list()
  
  for (i in seq_along(header_cells)) {
    header_text <- rvest::html_text2(header_cells[[i]])
    header_text <- trimws(header_text)
    
    # Find Club column
    if (grepl("^Club$", header_text, ignore.case = TRUE)) {
      col_positions$Club <- i
    }
  }
  
  # Verify required columns are found
  if (is.null(col_positions$Club)) {
    stop("Club column not found in table header for race_id '", race_id, "'")
  }
  
  # Step 5.2: Extract data from each row
  xpath_body_rows <- paste0("//table[@id='", race_id, "']/tbody/tr")
  body_rows <- html_content %>%
    rvest::html_elements(xpath = xpath_body_rows)
  
  if (length(body_rows) == 0) {
    # Return empty tibble with correct structure
    return(tibble::tibble(
      Club = character(0),
      `Profile URL` = character(0)
    ))
  }
  
  # Initialize result vectors
  n_rows <- length(body_rows)
  Club <- character(n_rows)
  `Profile URL` <- character(n_rows)
  
  # Extract data from each row
  for (row_idx in seq_along(body_rows)) {
    tr <- body_rows[[row_idx]]
    
    # Extract Club
    xpath_club <- paste0(".//td[", col_positions$Club, "]")
    club_cell <- tr %>%
      rvest::html_element(xpath = xpath_club)
    if (!is.null(club_cell)) {
      Club[row_idx] <- rvest::html_text2(club_cell) %>% trimws()
    }
    
    # Extract Profile URL (from Club column, link to /groups/)
    xpath_profile <- paste0(".//td[", col_positions$Club, "]/a[contains(@href, '/groups/')]")
    profile_link <- tr %>%
      rvest::html_element(xpath = xpath_profile)
    if (!is.null(profile_link)) {
      `Profile URL`[row_idx] <- rvest::html_attr(profile_link, "href")
    }
  }
  
  # Step 6: Create tibble and remove duplicates
  result <- tibble::tibble(
    Club = Club,
    `Profile URL` = `Profile URL`
  )
  
  # Step 6: Remove duplicate rows (many racers can ski for a club)
  # Remove rows where both Club and Profile URL are empty/NA
  result <- result[!(is.na(result$Club) | trimws(result$Club) == "") & 
                   !(is.na(result$`Profile URL`) | trimws(result$`Profile URL`) == ""), ]
  
  # Remove duplicates based on Profile URL (most reliable identifier)
  if (nrow(result) > 0) {
    result <- result[!duplicated(result$`Profile URL`), ]
  }
  
  # Step 7: Return the de-duplicated tibble
  return(result)
}

