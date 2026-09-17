# Multiple comparison procedure

Performs a one-way analysis of variance followed by an appropriate
post-hoc multiple pairwise comparison based on the distributional
characteristics and design of the data. The procedure evaluates
normality, homogeneity of variances, and sample-size balance to select
an appropriate analysis protocol.

## Usage

``` r
pairwise_comparison(
  data,
  formula,
  alpha = 0.05,
  rounding = 4,
  verbose = TRUE,
  p_adjust_method = "holm"
)
```

## Arguments

- data:

  A data frame containing the response and grouping variables.

- formula:

  A two-sided formula specifying the response and grouping variables in
  the form of `response ~ group`.

- alpha:

  A numeric significance level used for hypothesis testing. The default
  is 0.05.

- rounding:

  An integer specifying the number of decimal places used when reporting
  numerical results. The default is 4.

- verbose:

  A logical value indicating whether the results should be printed to
  the console. If `TRUE`, the default, the selected analysis protocol,
  post-hoc comparisons, and summary are printed. If `FALSE`, no output
  is printed.

- p_adjust_method:

  A character string specifying the method used to adjust p-values for
  Dunn's multiple-comparison test when the Kruskal-Wallis/Dunn protocol
  is selected. The default is "holm". See
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  available methods.

## Value

A list containing the results of the selected analysis protocol. The
method component identifies the complete protocol, combining the *a
priori* and post-hoc procedures. The object also contains the post-hoc
results and summary produced by the selected procedure.

## Details

If the response variable is normally distributed, the post-hoc procedure
is selected according to variance homogeneity and sample-size balance:

- equal variances and balanced group sizes: REGWQ test;

- equal variances and unbalanced group sizes: Tukey-Kramer test;

- unequal variances: Games-Howell test.

If the response variable is not normally distributed, an
aligned-rank-transform (ART) analysis is first attempted. Normality and
variance homogeneity are then reassessed using the aligned-ranked
response. If the aligned-ranked response is normally distributed, the
same variance- and balance-based selection described above is applied.
Otherwise, the procedure uses the Kruskal-Wallis test followed by Dunn's
multiple-comparison test.

The resulting object contains the selected pre-hoc and post-hoc analysis
results and identifies the complete analysis protocol in the method
component.

The analysis proceeds according to the following decision sequence:

1.  A preliminary one-way ANOVA is performed to assess normality and
    homoscedasticity.

2.  If the response is normally distributed, the post-hoc procedure is
    selected according to variance homogeneity and sample-size balance.

3.  If the response is not normally distributed, an ART analysis is
    performed.

4.  Normality and variance homogeneity are reassessed using the
    aligned-ranked response from the ART analysis.

5.  If the aligned-ranked response remains non-normal, the
    Kruskal-Wallis test followed by Dunn's test is used.

6.  Otherwise, the REGWQ, Tukey-Kramer, or Games-Howell procedure is
    selected according to variance homogeneity and sample-size balance.

Thus, the possible analysis protocols are:

- one-way ANOVA + REGWQ;

- one-way ANOVA + Tukey-Kramer;

- one-way ANOVA + Games-Howell;

- ART-ANOVA + REGWQ;

- ART-ANOVA + Tukey-Kramer;

- ART-ANOVA + Games-Howell;

- Kruskal-Wallis + Dunn's test.

The function is intended as a general-purpose adaptive procedure for
one-factor experimental designs. The selected method depends on the
diagnostic results obtained from the supplied data and should therefore
be interpreted together with the underlying assumptions and study
design.

## See also

[`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md),
[`oneway_art()`](https://p10911004-npust.github.io/oneway/reference/oneway_art.md),
[`REGWQ_test()`](https://p10911004-npust.github.io/oneway/reference/REGWQ_test.md),
[`Tukey_Kramer_test()`](https://p10911004-npust.github.io/oneway/reference/Tukey_Kramer_test.md),
[`Games_Howell_test()`](https://p10911004-npust.github.io/oneway/reference/Games_Howell_test.md),
[`Kruskal_Wallis_test()`](https://p10911004-npust.github.io/oneway/reference/Kruskal_Wallis_test.md),
[`Dunn_test()`](https://p10911004-npust.github.io/oneway/reference/Dunn_test.md)

## Examples

``` r
# Normally distributed, variance equal, balanced groups
pairwise_comparison(O_O_O, val ~ grp)
#> 
#> ------------------------------------
#> Fisher's ANOVA +
#> REGWQ multiple comparison procedure
#> ------------------------------------
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

# Normally distributed, variance equal, unbalanced groups
pairwise_comparison(O_O_X, val ~ grp)
#> 
#> -------------------------------------------
#> Fisher's ANOVA +
#> Tukey-Kramer multiple comparison procedure
#> -------------------------------------------
#> Data: O_O_X ; Formula: val ~ grp
#> 
#>   GROUP CLD  N    AVG     SD    MED
#> 1    G1   a 27 5.9377 0.9802 5.7820
#> 2    G2   a 24 6.1523 0.8466 6.0457
#> 3    G3   c 16 3.0106 0.8171 3.0477
#> 4    G4   b 30 5.2209 0.9513 5.2100
#> 5    G5   d 11 1.6157 0.8302 1.6525
#> 6    G6   c 20 3.8619 0.7809 3.8442
#> 

# Normally distributed, variance unequal
pairwise_comparison(O_X_X, val ~ grp)
#> 
#> -------------------------------------------
#> Welch's ANOVA +
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

# Non-parametric
pairwise_comparison(X_X_X, val ~ grp)
#> 
#> -------------------------------------
#> Kruskal-Wallis +
#> Dunn's multiple comparison procedure
#> -------------------------------------
#> Data: X_X_X ; Formula: val ~ grp
#> 
#>   GROUP CLD  N     AVG     SD     MED
#> 1    G1   a 27  9.6673 1.8475  9.8372
#> 2    G2  bc 24  4.3029 2.2668  3.7481
#> 3    G3   d 16  1.5668 0.4797  1.4076
#> 4    G4  ab 16  6.4910 1.2142  6.4238
#> 5    G5   a 11 11.8238 3.6990 11.1457
#> 6    G6 bcd 16  3.7907 1.2361  3.5400
#> 
```
