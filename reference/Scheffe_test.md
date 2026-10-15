# Scheffé's Method for Planned Contrasts

Performs Scheffé's multiple-comparison procedure for testing planned
contrasts among group means following an analysis of variance (ANOVA).
By default, all pairwise comparisons are performed (NOT recommended,
please use other tests to conduct pairwise comparison). Users can also
specify custom contrasts through the `planned_contrasts` argument.

## Usage

``` r
Scheffe_test(
  data,
  formula = NULL,
  alpha = 0.05,
  rounding = 4,
  verbose = TRUE,
  planned_contrasts = NULL
)
```

## Arguments

- data:

  A data frame, a list, or an object returned by
  [`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md),
  from which the response and grouping variables are obtained.

- formula:

  A two-sided formula specifying the response and grouping variables in
  the form `response ~ group`. Ignored if `data` is a list or an
  `"oneway_aov"` object.

- alpha:

  A numeric value between 0 and 1 (default: 0.05) specifying the
  significance level.

- rounding:

  An integer (default: 4). The number of decimal places to display in
  the output.

- verbose:

  Logical (default: TRUE). Show warnings and messages.

- planned_contrasts:

  An optional contrast matrix. For example, see
  [`pairwise_contrasts()`](https://p10911004-npust.github.io/oneway/reference/pairwise_contrasts.md).
  Each row represents a contrast, and each column corresponds to a
  group. If `NULL`, all pairwise contrasts are generated automatically.
  Custom contrasts should have coefficients that define the intended
  linear comparisons among group means.

## Value

An object containing the results of the Scheffé planned-contrast
analysis, including the underlying ANOVA results, group summaries,
post-hoc comparison results, and contrast matrix. The object is returned
invisibly.

## References

Heckert, N.A., Filliben, J.J., Croarkin, C.M., Hembree, B., Guthrie,
W.F., Tobias, P., & Prinz, J. (2002). Handbook 151: Nist/sematech
e-handbook of statistical methods. Section 7.4.6.2: Scheffe's method
https://doi.org/10.18434/M32189

Howell, D.C. (2010). Statistical Methods for Psychology (7th ed.).
Cengage Learning. Chapter 12: Multiple comparisons among treatment
means, Section 12.3: A Priori Comparisons, pg. 374.

Montgomery, D.C. (2017). Design and analysis of experiments (9th ed.).
John Wiley & Sons. pg. 89-95.

Scheffé, H. (1953). A method for judging all contrasts in the analysis
of variance. Biometrika, 40, 87–104.
https://doi.org/10.1093/biomet/40.1-2.87

## See also

[`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md),
[`pairwise_contrasts()`](https://p10911004-npust.github.io/oneway/reference/pairwise_contrasts.md)

## Examples

``` r
# Compare all pairwise group means using Scheffé's method
out <- Scheffe_test(morphine, tolerance ~ grp)
#> 
#> -----------------------------------
#> Scheffe's planned contrasts method
#> -----------------------------------
#> Data: morphine ; Formula: tolerance ~ grp
#> 
#>     x1  x2 [x1 - x2] Pvalue signif     [x1 - x2] 95% CI      F  Fcrit StdErr
#> 1   MM  MS         6 0.0049     **   [-3.1939, 15.1939]  4.500 3.2505 2.8284
#> 2   MM McM       -19 0.0000    ***  [-28.1939, -9.8061] 45.125 3.2505 2.8284
#> 3   MM  SM       -14 0.0000    ***  [-23.1939, -4.8061] 24.500 3.2505 2.8284
#> 4   MM  SS        -1 0.9725     ns   [-10.1939, 8.1939]  0.125 3.2505 2.8284
#> 5   MS McM       -25 0.0000    *** [-34.1939, -15.8061] 78.125 3.2505 2.8284
#> 6   MS  SM       -20 0.0000    *** [-29.1939, -10.8061] 50.000 3.2505 2.8284
#> 7   MS  SS        -7 0.0008    ***   [-16.1939, 2.1939]  6.125 3.2505 2.8284
#> 8  McM  SM         5 0.0268      *   [-4.1939, 14.1939]  3.125 3.2505 2.8284
#> 9  McM  SS        18 0.0000    ***    [8.8061, 27.1939] 40.500 3.2505 2.8284
#> 10  SM  SS        13 0.0000    ***    [3.8061, 22.1939] 21.125 3.2505 2.8284
#> 

# Specify custom contrasts
grp_names <- c("MS", "MM", "SS", "SM", "McM")
ca <- c(-1, -1, -1, 1.5, 1.5)
cb <- c(0, -1, 0, 0, 1)
cc <- c(-1, 0, 1, 0, 0)
cd <- c(0, 1, -1, 0, 0)
contr <- matrix(c(ca, cb, cc, cd), ncol = length(grp_names), byrow = TRUE)
colnames(contr) <- grp_names
out <- Scheffe_test(morphine, tolerance ~ grp, planned_contrasts = contr)
#> 
#> -----------------------------------
#> Scheffe's planned contrasts method
#> -----------------------------------
#> Data: morphine ; Formula: tolerance ~ grp
#> 
#>         x1           x2 [x1 - x2] Pvalue signif   [x1 - x2] 95% CI       F
#> 1 McM + SM MM + MS + SS   18.1667 0.0000    *** [12.2321, 24.1013] 99.0083
#> 2      McM           MM   19.0000 0.0000    ***  [9.8061, 28.1939] 45.1250
#> 3       SS           MS    7.0000 0.0008    *** [-2.1939, 16.1939]  6.1250
#> 4       MM           SS   -1.0000 0.9725     ns [-10.1939, 8.1939]  0.1250
#>    Fcrit StdErr
#> 1 3.2505 1.8257
#> 2 3.2505 2.8284
#> 3 3.2505 2.8284
#> 4 3.2505 2.8284
#> 
```
