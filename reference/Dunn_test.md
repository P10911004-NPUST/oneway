# Dunn's multiple comparison test

Performs Dunn's multiple comparison procedure for all pairwise
comparisons of groups following a significant Kruskal-Wallis test.

## Usage

``` r
Dunn_test(
  data,
  formula = NULL,
  alpha = 0.05,
  p_adjust_method = "holm",
  rounding = 4,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame, or an object returned by
  [`Kruskal_Wallis_test()`](https://p10911004-npust.github.io/oneway/reference/Kruskal_Wallis_test.md),
  from which the response and grouping variables are obtained.

- formula:

  A two-sided formula specifying the response and grouping variables in
  the form `response ~ group`. Ignored if `data` is a `"oneway_aov"`
  object.

- alpha:

  A numeric value between 0 and 1 specifying the significance level. The
  default is `0.05`.

- p_adjust_method:

  A character string specifying the method used to adjust p-values for
  multiple comparisons. Must be one of
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html).
  The default is `"holm"`.

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

  The Kruskal-Wallis test results.

- post_hoc:

  Results of all pairwise Dunn comparisons, including differences in
  mean ranks, confidence intervals, Z statistics, unadjusted and
  adjusted p-values, and effect sizes.

- summary:

  Descriptive statistics for each group, including compact letter
  displays (CLD).

## Details

Dunn's test is a nonparametric post hoc procedure that compares group
mean ranks after a significant Kruskal-Wallis test. The test statistic
follows a normal approximation with an adjustment for tied ranks when
ties are present.

Because multiple pairwise comparisons are performed, p-values should be
adjusted to control the family-wise error rate or false discovery rate.
The adjustment method is specified by `p_adjust_method` and is passed to
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

If the data satisfy the assumptions of one-way ANOVA, parametric
procedures such as
[`Tukey_HSD_test()`](https://p10911004-npust.github.io/oneway/reference/Tukey_HSD_test.md),
[`REGWQ_test()`](https://p10911004-npust.github.io/oneway/reference/REGWQ_test.md),
or
[`Games_Howell_test()`](https://p10911004-npust.github.io/oneway/reference/Games_Howell_test.md)
may be more appropriate.

## References

Dinno, A. (2015). Nonparametric Pairwise Multiple Comparisons in
Independent Groups using Dunn’s Test. The Stata Journal: Promoting
Communications on Statistics and Stata, 15(1), 292-300.
https://doi.org/10.1177/1536867X1501500117

Zar, J. H. (2014). Biostatistical analysis (5th edition). Pearson.
Chapter 11: Multiple comparisons, pg. 255-256.

## Examples

``` r
out <- Dunn_test(X_X_O, val ~ grp)
#> 
#> -------------------------------------
#> Dunn's multiple comparison procedure
#> -------------------------------------
#> Data: X_X_O ; Formula: val ~ grp
#> 
#>   GROUP CLD  N     AVG     SD     MED
#> 1    G1   a 20 11.9633 0.7985 11.9720
#> 2    G2  bc 20  8.1905 2.5801  8.8330
#> 3    G3   d 20  5.5526 0.5113  5.3966
#> 4    G4   b 20  9.7142 1.3761  9.5068
#> 5    G5  bc 20  9.2874 1.1111  9.1363
#> 6    G6  cd 20  7.7708 4.9047  6.9180
#> 
```
