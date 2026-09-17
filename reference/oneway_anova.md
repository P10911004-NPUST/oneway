# One-Way Analysis of Variance

Performs a one-way analysis of variance (ANOVA) to compare the means of
two or more independent groups. By default, the function automatically
selects between the classical Fisher ANOVA and Welch's ANOVA according
to whether the group variances are judged to be homogeneous.

## Usage

``` r
oneway_anova(
  data,
  formula,
  alpha = 0.05,
  var_equal = NA,
  rounding = 4,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing the response and grouping variables.

- formula:

  A two-sided formula specifying the response and grouping variables.

- alpha:

  Numeric (default: 0.05). Significance level (range from 0 to 1).

- var_equal:

  A logical value indicating whether equal variances should be assumed.
  If `TRUE`, Fisher's ANOVA is performed. If `FALSE`, Welch's ANOVA is
  performed. If `NA` (default), equality of variances is determined
  automatically using
  [`varequal::is_var_equal()`](https://rdrr.io/pkg/varequal/man/is_var_equal.html).

- rounding:

  Integer (default: 4). Number of decimal places displayed in the
  output.

- verbose:

  Logical (default: TRUE). Show warnings and messages.

## Value

A data frame representing the ANOVA table with the following columns:

- DF:

  Degrees of freedom.

- SS:

  Sum of squares.

- MS:

  Mean square.

- Fvalue:

  Observed F statistic.

- Fcrit:

  Critical F value at the specified significance level.

- Pvalue:

  P-value associated with the F statistic.

- signif:

  Significance code corresponding to the p-value.

- p_omega2:

  Effect size. Partial omega squared.

- method:

  Show whether Fisher's or Welch's ANOVA was conducted.

The rows correspond to the treatment groups ("Group"), residual error
("Residuals"), and total variation ("Total").

## Details

If `var_equal = TRUE`, Fisher's ANOVA is performed. If
`var_equal = FALSE`, Welch's ANOVA is performed. When `var_equal = NA`
(the default), homogeneity of variances is assessed using
[`varequal::is_var_equal()`](https://rdrr.io/pkg/varequal/man/is_var_equal.html),
and the appropriate test is selected automatically.

Fisher's ANOVA assumes independent observations, normally distributed
populations, and equal population variances. When the equal-variance
assumption is violated, Welch's ANOVA provides a more robust alternative
by adjusting the test statistic and denominator degrees of freedom.

## References

Howell, D. C. (2013). Statistical methods for psychology (8th ed.).
Cengage Learning. Chapter 11, pg. 325-345.

Montgomery, D. C. (2017). Design and analysis of experiments (Ninth
edition). John Wiley & Sons, Inc.

## Examples

``` r
# Automatically select the appropriate procedure
oneway_anova(O_X_X, val ~ grp)
#>                DF        SS       MS   Fvalue Fcrit Pvalue signif p_omega2
#> Group      5.0000 1495.2103 299.0421 282.3221 2.422      0    ***    0.965
#> Residuals 45.0276   47.6943   1.0592       NA    NA     NA   <NA>       NA
#> Total     50.0276 2307.2441       NA       NA    NA     NA   <NA>       NA
#>                  method
#> Group     Welch's ANOVA
#> Residuals Welch's ANOVA
#> Total     Welch's ANOVA

# Classical one-way ANOVA
oneway_anova(O_O_X, val ~ grp, var_equal = TRUE)
#>            DF       SS      MS  Fvalue  Fcrit Pvalue signif p_omega2
#> Group       5 264.1782 52.8356 67.0058 2.2886      0    ***   0.7205
#> Residuals 122  96.1999  0.7885      NA     NA     NA   <NA>       NA
#> Total     127 360.3780      NA      NA     NA     NA   <NA>       NA
#>                   method
#> Group     Fisher's ANOVA
#> Residuals Fisher's ANOVA
#> Total     Fisher's ANOVA

# Welch's ANOVA
oneway_anova(O_X_X, val ~ grp, var_equal = FALSE)
#>                DF        SS       MS   Fvalue Fcrit Pvalue signif p_omega2
#> Group      5.0000 1495.2103 299.0421 282.3221 2.422      0    ***    0.965
#> Residuals 45.0276   47.6943   1.0592       NA    NA     NA   <NA>       NA
#> Total     50.0276 2307.2441       NA       NA    NA     NA   <NA>       NA
#>                  method
#> Group     Welch's ANOVA
#> Residuals Welch's ANOVA
#> Total     Welch's ANOVA
```
