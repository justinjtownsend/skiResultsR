#' Process multiple race event files
#'
#' This function processes multiple HTML files containing ski race results,
#' extracting race tables from each file and organizing them into a structured format.
#'
#' @param directory Path to the directory containing race result HTML files
#' @return A list of lists, where each inner list contains:
#'   - file: The name of the processed file
#'   - tables: A list of race tables extracted from the file
#' @export
process_raceEvent_files <- function(directory) {
  # Get list of HTML files in the directory
  files <- list.files(directory, pattern = "\\.html$", full.names = TRUE)
  
  # Process each file
  lapply(files, function(file) {
    list(
      file = basename(file),
      tables = extract_raceTables(file)
    )
  })
} 