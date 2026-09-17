# Descriptive statistics

Compute and summarize descriptive statistics for one or more groups.

## Usage

``` r
describe(data, formula, rounding = 2)
```

## Arguments

- data:

  A data frame or a list.

- formula:

  A formula with a dependent variable (DV) and an independent variable
  (IV). For example: `DV ~ IV`.

- rounding:

  Integer (default: 2). Rounding digits.

## Value

A data frame with 13 columns:

- GROUP:

  Group name.

- CLD:

  Compact letter display for multiple comparisons. This column is
  returned as an empty character vector and is intended to be filled by
  post hoc comparison functions. See
  [`oneway::compact_letter_display`](https://p10911004-npust.github.io/oneway/reference/compact_letter_display.md).

- N:

  Sample size.

- AVG:

  Arithmetic mean.

- SD:

  Sample standard deviation.

- MED:

  Median.

- MIN:

  Minimum observed value.

- MAX:

  Maximum observed value.

- CI (95%):

  Two-sided 95% confidence interval for the population mean.

- SKEW (= 0):

  Normal distribution has a skewness of 0. See
  [`normality::skewness`](https://rdrr.io/pkg/normality/man/skewness.html).

- KURT (= 3):

  Normal distribution has a kurtosis of 3. See
  [`normality::kurtosis`](https://rdrr.io/pkg/normality/man/kurtosis.html).

- normality:

  Is the data normally distributed? See
  [`normality::is_normal`](https://rdrr.io/pkg/normality/man/is_normal.html).

- n_outliers:

  Number of possible outliers. See
  [`outlying::Grubbs_test`](https://rdrr.io/pkg/outlying/man/Grubbs_test.html).

## Examples

``` r
y1 <- c(stats::rnorm(20), 7)
y2 <- c(stats::rnorm(22), -7, 9)
describe(list("apple" = y1, "banana" = y2))
#>    GROUP CLD  N  AVG   SD   MED   MIN MAX      CI (95%) SKEW (= 0) KURT (= 3)
#> 1  apple     21 0.04 1.94 -0.24 -2.44   7 [-0.84, 0.92]       2.37      11.25
#> 2 banana     24 0.07 2.54  0.03 -7.00   9    [-1, 1.15]       1.02      11.56
#>   normality n_outliers
#> 1     FALSE          1
#> 2     FALSE          2
```
