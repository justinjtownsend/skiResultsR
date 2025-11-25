# Extract event details from an HTML file

This function extracts basic event information from a skiresults.co.uk
HTML file using the details table. It returns a tibble with event
metadata including title, date (in ISO format), slope, slope URL,
format, and status.

## Usage

``` r
get_event_dtls(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A tibble with columns:

- title:

  Event title from HTML head

- date:

  Event date in ISO format (YYYY-MM-DD)

- slope:

  Slope/venue name

- slope_url:

  URL to the slope page

- format:

  Event format (e.g., "LSERSA")

- status:

  Event status (e.g., "Results Available")

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract event details
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
event_dtls <- get_event_dtls(file_path)

print(event_dtls$title)
print(event_dtls$date)
print(event_dtls$slope)
} # }
```
