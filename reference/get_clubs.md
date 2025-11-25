# Extract all clubs from an event HTML file

This function extracts all clubs by finding links with href="/groups/"
and returns structured data with core club information and URLs. Ensures
unique rows only.

## Usage

``` r
get_clubs(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A data frame containing:

- name:

  Club name

- abbreviation:

  Club 3-letter abbreviation (if available)

- href:

  Club URL (href to their group page)

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract all clubs from an event
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
clubs <- get_clubs(file_path)

head(clubs)
nrow(clubs)  # Number of unique clubs
} # }
```
