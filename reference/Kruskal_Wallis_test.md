# Kruskal–Wallis one-way analysis of variance by ranks

Performs the Kruskal–Wallis rank-sum test for comparing the
distributions of two or more independent groups. The test is a
nonparametric alternative to one-way ANOVA and is based on the ranks of
the observations rather than their original values.

## Usage

``` r
Kruskal_Wallis_test(data, formula, alpha = 0.05, rounding = 4, verbose = TRUE)
```

## Arguments

- data:

  A data frame containing the response and grouping variables.

- formula:

  A two-sided formula specifying the response and grouping variables.

- alpha:

  Numeric (default: 0.05). Significance level (range from 0 to 1).

- rounding:

  Integer (default: 4). Number of decimal places displayed in the
  output.

- verbose:

  Logical (default: TRUE). Show warnings and messages.

## Value

A data frame summarizing the Kruskal–Wallis test result in an ANOVA-like
table:

- DF:

  Degrees of freedom.

- SS:

  Rank-based between-group sum of squares. Residual and total sums of
  squares are not defined for the classical Kruskal–Wallis test and are
  therefore returned as NA.

- MS:

  Rank-based mean square (SS / DF). Only the between-group value is
  defined.

- H:

  Observed Kruskal–Wallis H statistic.

- Hcrit:

  Critical chi-squared value at the specified significance level.

- Pvalue:

  P-value associated with the H statistic.

- signif:

  Significance code corresponding to the p-value.

- p_omega2:

  Effect size (currently returned as NA).

- method:

  Statistical method ("Kruskal-Wallis").

The rows correspond to the between-group effect ("Group"), residuals
("Residuals"), and total ("Total"). Only the between-group row
contributes to the Kruskal–Wallis test statistic.

## Details

The Kruskal–Wallis test ranks all observations across groups and tests
whether the mean ranks differ among groups. Under the null hypothesis,
the test statistic approximately follows a chi-squared distribution with
k - 1 degrees of freedom, where k is the number of groups. A correction
for tied ranks is applied when ties are present.

Unlike classical one-way ANOVA, the Kruskal–Wallis test is not derived
from a decomposition of variance into between-group and within-group
sums of squares. Consequently, residual sums of squares and residual
mean squares are not defined in the classical procedure.

## References

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion
variance analysis. Journal of the American Statistical Association,
47(260), 583–621.

Howell, D. C. (2013). Statistical methods for psychology (8th edition).
Cengage. Chapter 18, Section 18.9, pg. 678-679.

Hollander, M., Wolfe, D. A., & Chicken, E. (2014). Nonparametric
Statistical Methods (3rd ed.). Wiley. Chapter 6, pg. 204-206.

## Examples

``` r
lst <- list(
    "depressant" = c(55, 0, 1, 0, 50, 60, 44),
    "stimulant" = c(73, 85, 51, 63, 85, 85, 66, 69),
    "placebo" = c(61, 54, 80, 47)
)
normality::is_normal(lst)
#> [1] FALSE
Kruskal_Wallis_test(lst)
#>           DF      SS MS       H  Hcrit Pvalue signif p_omega2         method
#> Group      2 328.125 NA 10.4075 5.9915 0.0055     **       NA Kruskal-Wallis
#> Residuals 16      NA NA      NA     NA     NA   <NA>       NA Kruskal-Wallis
#> Total     18      NA NA      NA     NA     NA   <NA>       NA Kruskal-Wallis

Kruskal_Wallis_test(anorexia, weight_gain ~ therapy)
#>           DF       SS MS      H  Hcrit Pvalue signif p_omega2         method
#> Group      2 3973.747 NA 9.0731 5.9915 0.0107      *       NA Kruskal-Wallis
#> Residuals 69       NA NA     NA     NA     NA   <NA>       NA Kruskal-Wallis
#> Total     71       NA NA     NA     NA     NA   <NA>       NA Kruskal-Wallis
```
