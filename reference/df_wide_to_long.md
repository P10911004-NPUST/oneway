# Convert a data frame from wide to long format

Reshapes a data frame from wide format to long format by stacking one or
more columns into a pair of key-value columns. This increases the number
of rows while reducing the number of columns. For more advanced
reshaping operations, consider using `tidyr::pivot_longer()`.

## Usage

``` r
df_wide_to_long(df, columns, names_to = "grp", values_to = "val", keep = FALSE)
```

## Arguments

- df:

  A data frame.

- columns:

  A numeric or character vector specifying the columns to pivot into
  long format.

- names_to:

  A character string specifying the name of the new column containing
  the original column names. Default is `"grp"`.

- values_to:

  A character string specifying the name of the new column containing
  the values from the pivoted columns. Default is `"val"`.

- keep:

  Logical. If `TRUE`, columns not specified in `columns` are retained in
  the output. If `FALSE` (default), only the pivoted columns are
  returned.

## Value

A data frame in long format. The output contains one column storing the
original column names (`names_to`) and another storing the corresponding
values (`values_to`). If `keep = TRUE`, non-pivoted columns are
retained.

## Examples

``` r
n <- 10
df0 <- data.frame(
  G1 = stats::rnorm(n, 6, 1),
  G2 = stats::rnorm(n, 6, 1),
  G3 = stats::rnorm(n, 3, 1)
)

df_wide_to_long(df0, c("G1", "G2"))
#>    grp      val
#> 1   G1 5.950035
#> 2   G1 5.748517
#> 3   G1 6.444797
#> 4   G1 8.755418
#> 5   G1 6.046531
#> 6   G1 6.577709
#> 7   G1 6.118195
#> 8   G1 4.088280
#> 9   G1 6.862086
#> 10  G1 5.756763
#> 11  G2 5.793913
#> 12  G2 6.019178
#> 13  G2 6.029561
#> 14  G2 6.549828
#> 15  G2 3.725885
#> 16  G2 8.682557
#> 17  G2 5.638779
#> 18  G2 6.213356
#> 19  G2 7.074346
#> 20  G2 5.334912
```
