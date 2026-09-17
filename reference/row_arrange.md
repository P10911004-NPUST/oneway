# Rearrange rows by the order of one or more columns

Reorders the rows of a data frame according to user-specified ordering
of the values in one or more columns. If `by_order` is omitted, the
unique values in each selected column are sorted and used as the
ordering levels.

## Usage

``` r
row_arrange(data, cols, by_order)
```

## Arguments

- data:

  A data frame.

- cols:

  A character or numeric vector specifying the columns used to determine
  the row order. Character values are interpreted as column names, and
  numeric values as column indices.

- by_order:

  A list specifying the ordering of values for each column in `cols`.
  Each element should be a character vector containing the desired
  ordering of the corresponding column. If a single vector is supplied,
  it is treated as a list of length one. If omitted, the unique values
  in each column are sorted and used as the ordering.

## Value

A data frame with the same columns as `data`, but with rows reordered
according to the specified column orderings.

## Examples

``` r
fct_lvl <- c("A", "B", "C", "D")

df0 <- data.frame(
  C1 = c("B", "D", "A", "A", "B", "C", "D"),
  C2 = c("D", "C", "D", "A", "C", "B", "A"),
  C3 = 1:7
)

row_arrange(df0, 1:2, list(fct_lvl, fct_lvl))
#>   C1 C2 C3
#> 4  A  A  4
#> 3  A  D  3
#> 5  B  C  5
#> 1  B  D  1
#> 6  C  B  6
#> 7  D  A  7
#> 2  D  C  2

# Use the default ordering (sorted unique values)
row_arrange(df0, c("C2", "C3"))
#>   C1 C2 C3
#> 4  A  A  4
#> 7  D  A  7
#> 6  C  B  6
#> 2  D  C  2
#> 5  B  C  5
#> 1  B  D  1
#> 3  A  D  3
```
