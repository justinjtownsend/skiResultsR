# Extract event metadata from an HTML file

This function extracts event information from a skiresults.co.uk HTML
file. It returns structured data with the full event title, date, slope,
status, race types, and hyperlinks.

## Usage

``` r
get_event_summary(file_path)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

## Value

A list containing:

- title:

  Full event title

- date:

  Event date

- slope:

  Slope/venue name

- status:

  Event status

- race_types:

  List of race types with their hyperlinks

- event_url:

  Original event URL (if available)

- additional_details:

  Other event metadata found

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract event summary
file_path <- system.file("extdata", "chatham_oct2023.html", package = "skiresultsR")
event_summary <- get_event_summary(file_path)

print(event_summary$title)
print(event_summary$date)
print(event_summary$slope)

# View race types
lapply(event_summary$race_types, function(x) x$text)
} # }
```
