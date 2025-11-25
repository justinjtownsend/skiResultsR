# Extract a specific race table by race ID from an event HTML file

This is the main (workhorse) function for the package. It produces
understandable race results for any race by pre-processing HTML tables
and then using rvest::html_table() to produce the final data frame.

## Usage

``` r
get_race(file_path, race_id)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

- race_id:

  Race identifier in the format 'race-9973'

## Value

A data frame containing the race results

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract specific race by ID
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
race_data <- get_race(file_path, "race-9973")

head(race_data)
} # }
```
