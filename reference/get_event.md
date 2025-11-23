# Extract all event information from an HTML file

This is the main event extraction function that retrieves comprehensive
event data from a skiresults.co.uk HTML file. It extracts event
metadata, all race tables, racer information, points data, and club
information.

## Usage

``` r
get_event(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A list containing:

- event_summary:

  Event metadata (title, date, slope, status)

- races:

  List of all race tables with complete race information

- racers:

  Data frame of unique racers with their profile links

- race_points:

  Data frame of racer points across all categories

- clubs:

  Data frame of clubs mentioned in the event

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract complete event data
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiResultsR")
event_data <- get_event(file_path)

# Access different components
print(event_data$event_summary)
head(event_data$racers)
names(event_data$races)
} # }
```
