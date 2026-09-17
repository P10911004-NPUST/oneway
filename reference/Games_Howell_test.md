# Games-Howell multiple comparison test

Performs the Games-Howell multiple comparison procedure for all pairwise
comparisons of group means. The Games-Howell test does not assume equal
variances or equal sample sizes and is commonly used following a one-way
analysis of variance when the homogeneity of variance assumption is
violated.

## Usage

``` r
Games_Howell_test(
  data,
  formula = NULL,
  alpha = 0.05,
  rounding = 4,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame, or an object returned by
  [`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md),
  from which the response and grouping variables are obtained.

- formula:

  A two-sided formula specifying the response and grouping variables in
  the form `response ~ group`. Ignored if `data` is a `"oneway_aov"`
  object.

- alpha:

  A numeric value between 0 and 1 specifying the significance level. The
  default is `0.05`.

- rounding:

  An integer specifying the number of decimal places to display in the
  output. The default is `4`.

- verbose:

  Logical (default: TRUE). Show warnings and messages.

## Value

A list containing the following components:

- method:

  The name of the statistical procedure.

- data:

  The input data used in the analysis.

- pre_hoc:

  The one-way analysis of variance results.

- post_hoc:

  Results of all pairwise Games-Howell comparisons, including mean
  differences, confidence intervals, test statistics, adjusted p-values,
  and effect sizes.

- summary:

  Descriptive statistics for each group, including compact letter
  displays (CLD).

## Details

The Games-Howell test is based on the studentized range distribution and
uses Welch-Satterthwaite degrees of freedom for each pairwise
comparison. Unlike Tukey's HSD test, it does not require equal variances
or balanced sample sizes.

## References

Howell, D. C. (2013). Statistical Methods for Psychology (8th ed.).
Cengage Learning. Chapter 12: Multiple comparisons among treatment
means, Section 12.7: Tukey's test, pg. 395.

Zar, J. H. (2014). Biostatistical analysis (5th ed.). Pearson. Chapter
11: Multiple comparisons, pg. 246.

## Examples

``` r
out <- Games_Howell_test(O_X_X, val ~ grp)
#> 
#> -------------------------------------------
#> Games-Howell multiple comparison procedure
#> -------------------------------------------
#> Data: O_X_X ; Formula: val ~ grp
#> 
#>   GROUP CLD  N     AVG     SD     MED
#> 1    G1   a 27 15.8754 1.9604 15.5641
#> 2    G2  cd 24 10.4570 2.5398 10.1372
#> 3    G3  cd 16  9.0211 1.6341  9.0953
#> 4    G4   e 30  5.2209 0.9513  5.2100
#> 5    G5  bc 11 10.4630 3.3207 10.6098
#> 6    G6   b 20 13.8619 0.7809 13.8442
#> 
```
