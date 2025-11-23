# Function Examples

## Function Examples

This vignette demonstrates practical examples of using the skiResultsR
functions with sample data. These examples show you exactly how to use
each function and what to expect from the output.

### Setup

``` r
library(skiResultsR)

# Get path to sample data with fallback for development
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
if (!file.exists(file_path) || file_path == "") {
  file_path <- file.path("inst", "extdata", "chatham_oct2023.html")
}

cat("Using sample file:", basename(file_path), "\n")
#> Using sample file: chatham_oct2023.html
cat("File exists:", file.exists(file_path), "\n")
#> File exists: TRUE
```

### Example 1: Basic Time Cleaning

This example shows how race times are cleaned and standardized:

``` r
# Example time strings that might appear in race results
example_times <- c("1:23.45", "DNF", "2:15.30", "DNS", "1:45.20")

cat("Example 1: Cleaning individual race times\n")
#> Example 1: Cleaning individual race times
cat("----------------------------------------\n")
#> ----------------------------------------

# Note: clean_race_time is an internal function, but we can demonstrate the concept
for (i in seq_along(example_times)) {
  cat(sprintf("Original: %s\n", example_times[i]))
  
  # Simple time parsing demonstration
  if (toupper(example_times[i]) %in% c("DNF", "DNS")) {
    cat(sprintf("  Status: %s\n", toupper(example_times[i])))
    cat("  Cleaned: NA\n")
  } else if (grepl(":", example_times[i])) {
    parts <- strsplit(example_times[i], ":")[[1]]
    if (length(parts) == 2) {
      minutes <- as.numeric(parts[1])
      seconds <- as.numeric(parts[2])
      total_seconds <- minutes * 60 + seconds
      cat("  Status: FINISHED\n")
      cat(sprintf("  Cleaned: %.2f seconds\n", total_seconds))
    }
  }
  cat("----------------------------------------\n")
}
#> Original: 1:23.45
#>   Status: FINISHED
#>   Cleaned: 83.45 seconds
#> ----------------------------------------
#> Original: DNF
#>   Status: DNF
#>   Cleaned: NA
#> ----------------------------------------
#> Original: 2:15.30
#>   Status: FINISHED
#>   Cleaned: 135.30 seconds
#> ----------------------------------------
#> Original: DNS
#>   Status: DNS
#>   Cleaned: NA
#> ----------------------------------------
#> Original: 1:45.20
#>   Status: FINISHED
#>   Cleaned: 105.20 seconds
#> ----------------------------------------
```

### Example 2: Processing a Complete Event

Extract and examine complete event data:

``` r
if (!file.exists(file_path)) {
  cat("Sample file not available - skipping event processing example\n")
} else {
  cat("Example 2: Processing a complete event\n")
  cat("----------------------------------------\n")
  
  # Extract complete event data
  event_data <- get_event(file_path)
  event <- event_data[[1]]  # Get the inner element with class "skiResults_event"
  
  cat("Event Title:", event$event_dtls$title, "\n")
  cat("Number of races found:", length(event$races), "\n")
  if (!is.null(event$racers)) {
    cat("Number of unique racers:", nrow(event$racers), "\n")
  } else {
    cat("Number of unique racers: 0\n")
  }
  if (!is.null(event$points)) {
    total_points <- sum(sapply(event$points, function(x) if (!is.null(x)) nrow(x) else 0))
    cat("Number of points entries:", total_points, "\n")
  } else {
    cat("Number of points entries: 0\n")
  }
  if (!is.null(event$clubs)) {
    cat("Number of clubs:", nrow(event$clubs), "\n")
  } else {
    cat("Number of clubs: 0\n")
  }
}
#> Example 2: Processing a complete event
#> ----------------------------------------
#> Event Title: Chatham Ski Slope: LSERSA (07 October 2023) 
#> Number of races found: 5 
#> Number of unique racers: 97 
#> Number of points entries: 102 
#> Number of clubs: 10
```

### Example 3: Working with Individual Races

Extract and analyze individual race data:

``` r
if (!file.exists(file_path)) {
  cat("Sample file not available - skipping race analysis example\n")
} else {
  cat("Example 3: Working with individual races\n")
  cat("----------------------------------------\n")
  
  # Get all races
  races <- get_races(file_path)
  race_ids <- names(races)
  
  cat("Available races:\n")
  for (i in seq_along(race_ids)) {
    cat(sprintf("%d. %s\n", i, race_ids[i]))
  }
  
  if (length(race_ids) > 0) {
    # Get the first race as a clean data frame
    first_race <- get_race(file_path, race_ids[1])
    
    cat(sprintf("\nAnalyzing race: %s\n", race_ids[1]))
    cat(sprintf("Number of participants: %d\n", nrow(first_race)))
    cat(sprintf("Number of columns: %d\n", ncol(first_race)))
    cat("Column names:", paste(names(first_race), collapse = ", "), "\n")
    
    # Show first few rows
    if (nrow(first_race) > 0) {
      cat("\nFirst few participants:\n")
      print(head(first_race, 3))
    }
    
    # Analyze time data if available (look for "Overall Time" or similar columns)
    time_col <- NULL
    for (col in c("Overall Time", "overall_time", "time", "Time")) {
      if (col %in% names(first_race)) {
        time_col <- col
        break
      }
    }
    
    if (!is.null(time_col)) {
      time_values <- first_race[[time_col]]
      # Try to convert to numeric
      time_numeric <- suppressWarnings(as.numeric(time_values))
      finished_times <- time_numeric[!is.na(time_numeric)]
      
      if (length(finished_times) > 0) {
        cat("\nRace Statistics:\n")
        cat("- Participants with times:", length(finished_times), "\n")
        cat("- Fastest time:", min(finished_times), "\n")
        cat("- Slowest time:", max(finished_times), "\n")
        cat("- Average time:", round(mean(finished_times), 2), "\n")
      }
      
      # Count DNS, DNF, DSQ
      time_char <- as.character(time_values)
      dns_count <- sum(grepl("^DNS$", time_char, ignore.case = TRUE))
      dnf_count <- sum(grepl("^DNF$", time_char, ignore.case = TRUE))
      dsq_count <- sum(grepl("^DSQ$", time_char, ignore.case = TRUE))
      
      if (dns_count > 0 || dnf_count > 0 || dsq_count > 0) {
        cat("\nStatus Distribution:\n")
        cat("- DNS:", dns_count, "\n")
        cat("- DNF:", dnf_count, "\n")
        cat("- DSQ:", dsq_count, "\n")
      }
    }
  }
}
#> Example 3: Working with individual races
#> ----------------------------------------
#> Available races:
#> 1. race-9973
#> 2. race-9981
#> 3. race-9974
#> 4. race-9975
#> 5. race-9976
#> 
#> Analyzing race: race-9973
#> Number of participants: 97
#> Number of columns: 10
#> Column names: Rank, Bib, (Rk), Cat., Name, Club, Run 1, Run 2, Run 3, Overall Time 
#> 
#> First few participants:
#> # A tibble: 3 × 10
#>    Rank   Bib `(Rk)` Cat.     Name  Club  `Run 1` `Run 2` `Run 3` `Overall Time`
#>   <int> <int>  <int> <chr>    <chr> <chr> <chr>   <chr>   <chr>   <chr>         
#> 1     1   142      1 Male U21 BUNT… CHT   15.63   15.52   15.61   31.13         
#> 2     2   138      1 Male U18 BROW… BRO   16.29   DNF     16.63   32.92         
#> 3     3   136      2 Male U18 EVER… BRO   16.57   16.64   16.78   33.21         
#> 
#> Race Statistics:
#> - Participants with times: 90 
#> - Fastest time: 31.13 
#> - Slowest time: 74.57 
#> - Average time: 45.26 
#> 
#> Status Distribution:
#> - DNS: 4 
#> - DNF: 3 
#> - DSQ: 0
```

### Example 4: Extracting Participant Information

Get detailed information about race participants:

``` r
if (!file.exists(file_path)) {
  cat("Sample file not available - skipping participant example\n")
} else {
  cat("Example 4: Extracting participant information\n")
  cat("----------------------------------------\n")
  
  # First get race IDs to use for get_racers()
  races <- get_races(file_path)
  if (length(races) > 0) {
    # Get participant information for the first race
    race_id <- names(races)[1]
    participants <- get_racers(file_path, race_id = race_id)
    
    cat(sprintf("Total participants in race %s: %d\n", race_id, nrow(participants)))
    
    if (nrow(participants) > 0) {
      # Check for profile links
      if ("Profile URL" %in% names(participants)) {
        with_links <- sum(!is.na(participants$`Profile URL`) & participants$`Profile URL` != "")
        cat("Participants with profile links:", with_links, "\n")
      }
      
      # Check for club information
      if ("Club" %in% names(participants)) {
        with_clubs <- sum(!is.na(participants$Club) & participants$Club != "")
        cat("Participants with club information:", with_clubs, "\n")
      }
      
      cat("\nFirst few participants:\n")
      print(head(participants, 5))
      
      # Show unique clubs if any
      if ("Club" %in% names(participants)) {
        unique_clubs <- unique(participants$Club[!is.na(participants$Club) & participants$Club != ""])
        if (length(unique_clubs) > 0) {
          cat("\nClubs represented:\n")
          for (club in unique_clubs) {
            cat("-", club, "\n")
          }
        }
      }
    }
  } else {
    cat("No races found in file\n")
  }
}
#> Example 4: Extracting participant information
#> ----------------------------------------
#> Total participants in race race-9973: 97
#> Participants with profile links: 97 
#> Participants with club information: 91 
#> 
#> First few participants:
#> # A tibble: 5 × 7
#>   Rank  Bib   `(Rk)` Cat.     Name               `Profile URL`             Club 
#>   <chr> <chr> <chr>  <chr>    <chr>              <chr>                     <chr>
#> 1 1     142   1      Male U21 BUNTON Ryan        https://skiresults.co.uk… CHT  
#> 2 2     138   1      Male U18 BROWN Ben          https://skiresults.co.uk… BRO  
#> 3 3     136   2      Male U18 EVEREST Toby       https://skiresults.co.uk… BRO  
#> 4 4     135   3      Male U18 ATKINSON Liam      https://skiresults.co.uk… CHT  
#> 5 5     137   4      Male U18 COLLYER-TODD Lucas https://skiresults.co.uk… CHT  
#> 
#> Clubs represented:
#> - CHT 
#> - BRO 
#> - ASR 
#> - HEM 
#> - BOW 
#> - ESX 
#> - TSN 
#> - SPR 
#> - FLK 
#> - SAS
```

### Example 5: Getting Event Details

Extract just the event metadata:

``` r
if (!file.exists(file_path)) {
  cat("Sample file not available - skipping event details example\n")
} else {
  cat("Example 5: Extracting event details\n")
  cat("----------------------------------------\n")
  
  # Get event details using get_event_dtls()
  event_details <- get_event_dtls(file_path)
  
  if (!is.null(event_details) && nrow(event_details) > 0) {
    cat("Event details found:\n")
    
    # Show main details
    if (!is.na(event_details$title)) {
      cat("Title:", event_details$title, "\n")
    }
    if (!is.na(event_details$date)) {
      cat("Date:", event_details$date, "\n")
    }
    if (!is.na(event_details$slope)) {
      cat("Venue:", event_details$slope, "\n")
    }
    if (!is.na(event_details$format)) {
      cat("Format:", event_details$format, "\n")
    }
    if (!is.na(event_details$status)) {
      cat("Status:", event_details$status, "\n")
    }
  } else {
    cat("No event details found in the file\n")
  }
}
#> Example 5: Extracting event details
#> ----------------------------------------
#> Event details found:
#> Title: Chatham Ski Slope: LSERSA (07 October 2023) 
#> Date: 2023-10-07 
#> Venue: Chatham Ski Slope 
#> Format: LSERSA 
#> Status: Results Available
```

### Example 6: Points Analysis

Analyze points data for a specific race:

``` r
if (!file.exists(file_path)) {
  cat("Sample file not available - skipping points analysis example\n")
} else {
  cat("Example 6: Points analysis\n")
  cat("----------------------------------------\n")
  
  # First get race IDs
  races <- get_races(file_path)
  if (length(races) > 0) {
    # Get points for the first race
    race_id <- names(races)[1]
    points_data <- tryCatch({
      get_points(file_path, race_id = race_id)
    }, error = function(e) {
      # Points may not exist for all races
      NULL
    })
    
    if (!is.null(points_data) && nrow(points_data) > 0) {
      cat("Points entries found for", race_id, ":", nrow(points_data), "\n")
      
      # Show sample points data
      cat("\nSample points data:\n")
      print(head(points_data, 5))
      
      # Show column names to understand structure
      cat("\nPoints data columns:", paste(names(points_data), collapse = ", "), "\n")
    } else {
      cat("No points data found for", race_id, "\n")
      cat("(Points may not be available for all races)\n")
    }
  } else {
    cat("No races found in file\n")
  }
}
#> Example 6: Points analysis
#> ----------------------------------------
#> Points entries found for race-9973 : 97 
#> 
#> Sample points data:
#>   Rank Bib (Rk)     Cat.               Name
#> 1    1 142    1 Male U21        BUNTON Ryan
#> 2    2 138    1 Male U18          BROWN Ben
#> 3    3 136    2 Male U18       EVEREST Toby
#> 4    4 135    3 Male U18      ATKINSON Liam
#> 5    5 137    4 Male U18 COLLYER-TODD Lucas
#>   LSERSA Summer Series: Fastest Female LSERSA Summer Series: Fastest Male
#> 1                                 <NA>                             100.00
#> 2                                 <NA>                              94.25
#> 3                                 <NA>                              93.32
#> 4                                 <NA>                              92.52
#> 5                                 <NA>                              89.46
#>   LSERSA Summer Series: Female MAS1 LSERSA Summer Series: Female MAS2
#> 1                              <NA>                              <NA>
#> 2                              <NA>                              <NA>
#> 3                              <NA>                              <NA>
#> 4                              <NA>                              <NA>
#> 5                              <NA>                              <NA>
#>   LSERSA Summer Series: Female SEN LSERSA Summer Series: Female U10
#> 1                             <NA>                             <NA>
#> 2                             <NA>                             <NA>
#> 3                             <NA>                             <NA>
#> 4                             <NA>                             <NA>
#> 5                             <NA>                             <NA>
#>   LSERSA Summer Series: Female U12 LSERSA Summer Series: Female U14
#> 1                             <NA>                             <NA>
#> 2                             <NA>                             <NA>
#> 3                             <NA>                             <NA>
#> 4                             <NA>                             <NA>
#> 5                             <NA>                             <NA>
#>   LSERSA Summer Series: Female U16 LSERSA Summer Series: Female U18
#> 1                             <NA>                             <NA>
#> 2                             <NA>                             <NA>
#> 3                             <NA>                             <NA>
#> 4                             <NA>                             <NA>
#> 5                             <NA>                             <NA>
#>   LSERSA Summer Series: Female U21 LSERSA Summer Series: Female U8
#> 1                             <NA>                            <NA>
#> 2                             <NA>                            <NA>
#> 3                             <NA>                            <NA>
#> 4                             <NA>                            <NA>
#> 5                             <NA>                            <NA>
#>   LSERSA Summer Series: Male MAS1 LSERSA Summer Series: Male MAS2
#> 1                            <NA>                            <NA>
#> 2                            <NA>                            <NA>
#> 3                            <NA>                            <NA>
#> 4                            <NA>                            <NA>
#> 5                            <NA>                            <NA>
#>   LSERSA Summer Series: Male SEN LSERSA Summer Series: Male U10
#> 1                           <NA>                           <NA>
#> 2                           <NA>                           <NA>
#> 3                           <NA>                           <NA>
#> 4                           <NA>                           <NA>
#> 5                           <NA>                           <NA>
#>   LSERSA Summer Series: Male U12 LSERSA Summer Series: Male U14
#> 1                           <NA>                           <NA>
#> 2                           <NA>                           <NA>
#> 3                           <NA>                           <NA>
#> 4                           <NA>                           <NA>
#> 5                           <NA>                           <NA>
#>   LSERSA Summer Series: Male U16 LSERSA Summer Series: Male U18
#> 1                           <NA>                           <NA>
#> 2                           <NA>                          15.00
#> 3                           <NA>                          12.00
#> 4                           <NA>                          10.00
#> 5                           <NA>                           8.00
#>   LSERSA Summer Series: Male U21 LSERSA Summer Series: Male U8
#> 1                          15.00                          <NA>
#> 2                           <NA>                          <NA>
#> 3                           <NA>                          <NA>
#> 4                           <NA>                          <NA>
#> 5                           <NA>                          <NA>
#> 
#> Points data columns: Rank, Bib, (Rk), Cat., Name, LSERSA Summer Series: Fastest Female, LSERSA Summer Series: Fastest Male, LSERSA Summer Series: Female MAS1, LSERSA Summer Series: Female MAS2, LSERSA Summer Series: Female SEN, LSERSA Summer Series: Female U10, LSERSA Summer Series: Female U12, LSERSA Summer Series: Female U14, LSERSA Summer Series: Female U16, LSERSA Summer Series: Female U18, LSERSA Summer Series: Female U21, LSERSA Summer Series: Female U8, LSERSA Summer Series: Male MAS1, LSERSA Summer Series: Male MAS2, LSERSA Summer Series: Male SEN, LSERSA Summer Series: Male U10, LSERSA Summer Series: Male U12, LSERSA Summer Series: Male U14, LSERSA Summer Series: Male U16, LSERSA Summer Series: Male U18, LSERSA Summer Series: Male U21, LSERSA Summer Series: Male U8
```

### Summary

These examples demonstrate the key capabilities of the skiResultsR
package:

1.  **Complete event extraction** with
    [`get_event()`](https://justinjtownsend.github.io/skiResultsR/reference/get_event.md)
2.  **Individual race analysis** with
    [`get_race()`](https://justinjtownsend.github.io/skiResultsR/reference/get_race.md)
    and
    [`get_races()`](https://justinjtownsend.github.io/skiResultsR/reference/get_races.md)
3.  **Participant management** with
    [`get_racers()`](https://justinjtownsend.github.io/skiResultsR/reference/get_racers.md)
4.  **Event metadata** with
    [`get_event_dtls()`](https://justinjtownsend.github.io/skiResultsR/reference/get_event_dtls.md)
    and
    [`get_event_summary()`](https://justinjtownsend.github.io/skiResultsR/reference/get_event_summary.md)
5.  **Points analysis** with
    [`get_points()`](https://justinjtownsend.github.io/skiResultsR/reference/get_points.md)

Each function is designed to handle real-world ski race data and provide
clean, structured output for further analysis.

### Next Steps

- Try these functions with your own HTML files from skiresults.co.uk
- Combine the output with data visualization packages like `ggplot2`
- Use the structured data for statistical analysis of race performance
- Export results to CSV or other formats for sharing
