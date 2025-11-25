# Extract all registered racers from an event HTML file

This function extracts all racers by finding links with href="/people/"
and returns structured data with core racer information, racer URLs, and
club information. Ensures unique rows only.

## Usage

``` r
get_racers(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A data frame containing:

- name:

  Racer name (Lastname Firstname format)

- href:

  Racer URL (href to their profile page)

- club:

  Racer club (if available)

- club_href:

  Club hyperlink (if available)

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract all racers from an event
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
racers <- get_racers(file_path)

head(racers)
nrow(racers)  # Number of unique racers
} # }
```
