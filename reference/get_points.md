# Extract points data for a specific race

This function tracks the points gained by individual racers for a
specific race. It produces a data frame with one row per racer, with
columns expanded to include all points categories. Each row represents
one racer with their points spread across multiple columns (one per
points category).

## Usage

``` r
get_points(file_path, race_id)
```

## Arguments

- file_path:

  Path to the HTML file containing event results

- race_id:

  Race identifier in the format 'race-9973'

## Value

A data frame with one row per racer and columns for:

- Rank:

  Racer rank/position

- Bib:

  Racer bib number

- (Rk):

  Racer rank

- Cat.:

  Race category

- Name:

  Racer name

- Points Category 1:

  Points for first category

- Points Category 2:

  Points for second category

- ...:

  Additional points category columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract points for a specific race
file_path <- system.file("extdata", "brentwood_jun2023.html", package = "skiresultsR")
points_data <- get_points(file_path, "race-9711")

head(points_data)
nrow(points_data)  # Number of racers (one row per racer)
ncol(points_data)  # Number of columns (5 static + dynamic points columns)
} # }
```
