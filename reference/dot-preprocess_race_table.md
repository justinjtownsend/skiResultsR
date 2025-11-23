# Internal function to pre-process race table HTML

Applies the pre-processing rules for headers and body rows before using
rvest::html_table()

## Usage

``` r
.preprocess_race_table(table)
```

## Arguments

- table:

  HTML table element

## Value

Pre-processed HTML table element
