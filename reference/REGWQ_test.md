# Ryan-Einot-Gabriel-Welsch Studentized Range (REGWQ) test

Performs the Ryan-Einot-Gabriel-Welsch Studentized Range (REGWQ)
multiple comparison procedure for all pairwise comparisons of group
means following a one-way analysis of variance. REGWQ is a stepwise
procedure based on the studentized range distribution that generally
provides greater statistical power than Tukey's HSD while maintaining
strong control of the family-wise error rate under balanced designs.

## Usage

``` r
REGWQ_test(data, formula = NULL, alpha = 0.05, rounding = 4, verbose = TRUE)
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

  Results of all pairwise REGWQ comparisons, including mean differences,
  confidence intervals, studentized range statistics, p-values, modified
  significance levels, and effect sizes.

- summary:

  Descriptive statistics for each group, including compact letter
  displays (CLD).

## Details

The REGWQ procedure is a stepwise multiple comparison method based on
the studentized range distribution. Group means are first ordered, after
which pairwise comparisons are performed using significance levels that
depend on the number of ordered means spanned by each comparison. This
adaptive strategy generally yields greater statistical power than
Tukey's HSD while maintaining control of the family-wise error rate.

The procedure assumes that observations are independent, residuals are
approximately normally distributed, and population variances are equal.
It is intended primarily for balanced one-way designs. When sample sizes
are unequal but variances remain homogeneous, consider using
[`Tukey_Kramer_test()`](https://p10911004-npust.github.io/oneway/reference/Tukey_Kramer_test.md).
When variances are unequal, consider using
[`Games_Howell_test()`](https://p10911004-npust.github.io/oneway/reference/Games_Howell_test.md).

## References

Howell, D. C. (2010). Statistical Methods for Psychology (7th ed.).
Cengage Learning. Chapter 12: Multiple comparisons among treatment
means, Section 12.6: Post hoc comparisons, pg. 393-394.

## Examples

``` r
out <- REGWQ_test(morphine, tolerance ~ grp)
#> 
#> ------------------------------------
#> REGWQ multiple comparison procedure
#> ------------------------------------
#> Data: morphine ; Formula: tolerance ~ grp
#> 
#>   GROUP CLD N AVG     SD  MED
#> 1    MM   b 8  10 5.1270 10.5
#> 2    MS   c 8   4 3.1623  3.5
#> 3   McM   a 8  29 6.1644 28.5
#> 4    SM   a 8  24 6.3696 23.0
#> 5    SS   b 8  11 6.7188 10.5
#> 
```
