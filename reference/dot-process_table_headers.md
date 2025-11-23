# Internal function to process table headers

Rule 5.1: If header columns contain nesting, choose the element's inner
text Rule 5.2: If header column contains nesting and inner text includes
"Points", ignore this element and all child elements from further
processing Rule 5.3: If header column element's inner text is blank
(null), return "Win For" Rule 5.4: If the header contains elements where
the inner text is the same, add a sequence number to the end (e.g.
Bib.1, Bib.2, Competitor.1, Competitor.2)

## Usage

``` r
.process_table_headers(table)
```

## Arguments

- table:

  HTML table element

## Value

Table with processed headers
