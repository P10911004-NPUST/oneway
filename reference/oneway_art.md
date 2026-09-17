# One-Way Aligned Ranked Transformed Analysis of Variance (ART-ANOVA)

Performs a one-way analysis of variance (ANOVA) to compare the means of
two or more independent groups. By default, the function automatically
selects between the classical Fisher ANOVA and Welch's ANOVA according
to whether the group variances are judged to be homogeneous.

## Usage

``` r
oneway_art(
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

  A logical value passes into the
  [`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md)
  to perform regular ANOVA on the ranked-response variable.

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

- method:

  A character specifying this is an ART-ANOVA procedure.

The rows correspond to the treatment groups ("Group"), residual error
("Residuals"), and total variation ("Total").

## Details

If `var_equal = TRUE`, Fisher's ANOVA is performed on the transformed
response variable. If `var_equal = FALSE`, Welch's ANOVA is performed.
When `var_equal = NA` (the default), homogeneity of variances is
assessed using
[`varequal::is_var_equal()`](https://rdrr.io/pkg/varequal/man/is_var_equal.html),
and the appropriate test is selected automatically.

## References

Wobbrock, J. O., Findlater, L., Gergle, D., & Higgins, J. J. (2011). The
aligned rank transform for nonparametric factorial analyses using only
ANOVA procedures. Proceedings of the SIGCHI Conference on Human Factors
in Computing Systems, 2011, 143–146.
https://doi.org/10.1145/1978942.1978963

Elkin, L. A., Kay, M., Higgins, J. J., & Wobbrock, J. O. (2021). An
aligned rank transform procedure for multifactor contrast tests.
Proceedings of the 34th Annual ACM Symposium on User Interface Software
and Technology, 754–768. https://doi.org/10.1145/3472749.3474784

## Examples

``` r
normality::is_normal(anorexia, weight_gain ~ therapy)
#> [1] FALSE
oneway_art(anorexia, weight_gain ~ therapy)
#>           DF        SS        MS Fvalue  Fcrit Pvalue signif p_omega2    method
#> Group      2  3996.027 1998.0135  5.087 3.1296 0.0087     **    0.102 ART-ANOVA
#> Residuals 69 27100.973  392.7677     NA     NA     NA   <NA>       NA ART-ANOVA
#> Total     71 31097.000        NA     NA     NA     NA   <NA>       NA ART-ANOVA
```
