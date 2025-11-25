# skiResultsR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
# [![R-CMD-check](https://github.com/justinjtownsend/skiResultsR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/justinjtownsend/skiResultsR/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/skiResultsR)](https://CRAN.R-project.org/package=skiResultsR)
<!-- badges: end -->

`skiResultsR` provides a thoughtful set of functions to extract race data from HTML files generated at skiresults.co.uk. It is designed to work with an event, extracting all races, racers and their clubs. Other information about the event is also captured.

## Features

- **Event Extraction** - Get event data with a single function call
- **Race(s)** - Get individual race results for further analysis
- **Points** - Get points from (scoring) races
- **Racer / Club Information** - Get racer, club details

## Installation

```r
# Or install development version from GitHub
devtools::install_github("justinjtownsend/skiResultsR")
```
## Quick Start

```r
library(skiResultsR)

# Get path to sample data
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")

# Extract complete event data
event_data <- get_event(file_path)

# View event summary
print(event_data[[1]]$event_dtls$title)
#> [1] "Chatham Ski Slope: LSERSA (07 October 2023)"

# View available races
names(event_data[[1]]$races)
#> [1] "race-9973" "race-9981" "race-9974" "race-9975" "race-9976"

# Get racers
head(event_data[[1]]$racers)
```

## Main Functions

### Primary Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `get_event()` | Get complete event data | List with all components |
| `get_races()` | Get all race tables | List of race data frames |
| `get_race()` | Get specific race by ID/type | Single race data frame |

### Helper Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `get_racers()` | Get racers with links | Data frame |
| `get_points()` | Get points across races | Data frame |
| `get_event_summary()` | Get event statistics | List of data frames |
| `get_race_types()` | Get race tyes | Data frame |

## Usage Examples

### Get Event Summary

```r
# Quick event overview
summary <- get_event_summary(event_data[[1]])
cat("Total Racers:", summary[[1]]$race_summary$tot_racers)
cat("DNFs", summary[[1]]$race_summary$overall_time_dnf)
cat("Aldershot Racers:", summary[[1]]$race_participation$club_ASR)
cat("")
cat("Overall Fastest Time:", summary[[1]]$race_summary$overall_time_fastest)

```

### Analyze Race Performance

```r
# Get a specific race
race_data <- get_race(file_path, "race-9973")

```

### Extract Points Data

```r
# Get points across all categories
points <- get_points(file_path, "race-9973")

```

## Data Structure

The package returns consistent, well-structured data:

```r
event_data <- get_event(file_path)
str(event_data, max.level = 2)

#> List of 1
#>  $ 1319:List of 6
#>   ..$ event_dtls:'data.frame':	1 obs. of  6 variables:
#>   ..$ race_types: tibble [5 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ races     :List of 5
#>   ..$ racers    : tibble [97 × 7] (S3: tbl_df/tbl/data.frame)
#>   ..$ points    :List of 2
#>   ..$ clubs     : tibble [10 × 2] (S3: tbl_df/tbl/data.frame)
#>   ..- attr(*, "class")= chr [1:2] "skiResults_event" "list"

```

## Sample Data

The package includes sample HTML files for testing and examples:

- `chatham_oct2023.html` - Chatham Ski Slope event from October 2023
- `brentwood_jun2023.html` - Brentwood Park event from June 2023

```r
# List available sample files
list.files(system.file("extdata", package = "skiResultsR"))
```

## Documentation

- **Vignette**: [Getting Started with skiResultsR](articles/getting-started.html)
- **Function Reference**: [Reference Documentation](reference/index.html)
- **Package Website**: [skiResultsR Documentation](https://justinjtownsend.github.io/skiResultsR/)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

```r
citation("skiResultsR")
```

## Related Packages

- [`rvest`](https://rvest.tidyverse.org/) - Web scraping (used internally)
- [`xml2`](https://xml2.r-lib.org/) - XML/HTML parsing (used internally)
- [`dplyr`](https://dplyr.tidyverse.org/) - Data manipulation (recommended for analysis)
- [`ggplot2`](https://ggplot2.tidyverse.org/) - Data visualization (recommended for plotting)

---

**skiResultsR** - Making ski race data analysis accessible in R 🎿📊