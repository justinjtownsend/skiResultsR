# Extract all race tables from an event HTML file

This function extracts all race tables from a skiresults.co.uk event
HTML file. It returns structured data with core race information,
additional points, racer and club information for all races in the
event.

## Usage

``` r
get_races(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A list where each element is a race table containing:

- race_id:

  The race identifier (e.g., "race-9973")

- race_data:

  Data frame with race results

- metadata:

  Additional race metadata if available

A named list of data frames, where each element is named by race_id and
contains the race results from get_race()

## Examples
