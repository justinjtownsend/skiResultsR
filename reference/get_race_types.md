# Extract race types and event information from an HTML file

This function extracts race types and event ID from the tabs section of
a skiresults.co.uk HTML file. It identifies all available race types for
an event and returns structured data with event ID and race information.

## Usage

``` r
get_race_types(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A list with the event_id as the name, containing:

- race_types:

  A data frame with columns:

  - event_idThe event ID (numeric)

  - race_typeThe race type name (e.g., "Individual", "Club
    Performances")

  - race_idThe race ID (e.g., "race-9973")

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract race types from an event
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
race_types <- get_race_types(file_path)

# Access the event ID (name of the list)
names(race_types)

# Access race types
race_types[[1]]$race_types
} # }
```
