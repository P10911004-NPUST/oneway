# Tukey's Honestly Significant Difference (Tukey-HSD) test

Performs Tukey's Honestly Significant Difference (HSD) multiple
comparison procedure for all pairwise comparisons of group means
following a one-way analysis of variance. The Tukey-HSD test controls
the family-wise error rate and is appropriate when the assumptions of
normality, homogeneity of variances, and balanced group sizes are
reasonably satisfied.

## Usage

``` r
Tukey_HSD_test(
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

  Results of all pairwise Tukey-HSD comparisons, including mean
  differences, confidence intervals, studentized range statistics,
  adjusted p-values, and effect sizes.

- summary:

  Descriptive statistics for each group, including compact letter
  displays (CLD).

## Details

Tukey's HSD test is based on the studentized range distribution and
assumes equal variances across groups. It provides simultaneous
confidence intervals and adjusted \\p\\-values that control the
family-wise error rate for all pairwise comparisons.

This implementation is intended primarily for balanced one-way designs.
When sample sizes are unequal but variances remain homogeneous, consider
using
[`Tukey_Kramer_test()`](https://p10911004-npust.github.io/oneway/reference/Tukey_Kramer_test.md).
When variances are unequal, consider using
[`Games_Howell_test()`](https://p10911004-npust.github.io/oneway/reference/Games_Howell_test.md).

## References

Howell, D. C. (2013). Statistical Methods for Psychology (8th ed.).
Cengage Learning. Chapter 12: Multiple comparisons among treatment
means, Section 12.7: Tukey's test, pg. 394.

Zar, J. H. (2014). Biostatistical analysis (5th edition). Pearson.
Chapter 11: Multiple comparisons, pg. 241-243.

## Examples

``` r
out <- Tukey_HSD_test(O_O_O, val ~ grp)
#> 
#> ----------------------------------------
#> Tukey-HSD multiple comparison procedure
#> ----------------------------------------
#> Data: O_O_O ; Formula: val ~ grp
#> 
#>   GROUP CLD  N    AVG     SD    MED
#> 1    G1   b 20 4.6416 0.9727 4.6200
#> 2    G2   a 20 5.9487 0.8299 5.8601
#> 3    G3   c 20 3.1065 0.9573 2.9643
#> 4    G4   b 20 4.8801 0.9731 4.7882
#> 5    G5   a 20 6.3751 0.8290 6.3585
#> 6    G6   c 20 3.6406 0.7011 3.6361
#> 
```
