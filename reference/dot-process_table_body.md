# Internal function to process table body

Rule 6.1: If body rows contain nesting, choose the element's inner text
Rule 6.2: If body row contains nesting and element's inner text is blank
and children have the 'points-' class, ignore this element and all child
elements Rule 6.3: If body rows contain attribute 'class=win_for_n',
capture the element attribute text Rule 6.4: Insert captured element
attribute text into the empty element

## Usage

``` r
.process_table_body(table)
```

## Arguments

- table:

  HTML table element

## Value

Table with processed body
