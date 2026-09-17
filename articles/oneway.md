# oneway

## Overview

The `oneway` package provides an integrated workflow for one-factor
experimental designs. Its functionality covers:

- descriptive statistics;
- *a priori* test (omnibus test)
  - classical ANOVA
  - aligned rank transform ANOVA (`ART-ANOVA`)
  - Kruskal–Wallis test
- *Post hoc* test (Multiple-comparison procedures, MCP):
  - REGWQ
  - Tukey-HSD
  - Tukey-Kramer
  - Games-Howell
  - Dunn’s
- Effect sizes;
- a single adaptive workflow,
  [`pairwise_comparison()`](https://p10911004-npust.github.io/oneway/reference/pairwise_comparison.md),
  that selects an appropriate *a priori* + *post hoc* protocol from the
  observed distributional, variance, and design characteristics.

  

## Installation

Install the released version from
[CRAN](https://cran.r-project.org/package=oneway):

``` r

install.packages("oneway")
```

or the development version from
[GitHub](https://github.com/P10911004-NPUST/oneway):

``` r

if (!require("pak")) install.packages("pak")
pak::pak("P10911004-NPUST/oneway")
```

Then load the package:

``` r

library(oneway)

# For reproducibility
set.seed(123)
```

  

## Quick start

The core function of this package is
[`pairwise_comparison()`](https://p10911004-npust.github.io/oneway/reference/pairwise_comparison.md),
which provides out-of-the-box statistical inference for one-way designs.
It expects a data frame and a two-sided formula:

``` r

out <- pairwise_comparison(morphine, tolerance ~ grp)
#> 
#> ------------------------------------
#> Fisher's ANOVA +
#> REGWQ multiple comparison procedure
#> ------------------------------------
#> Data: morphine ; Formula: tolerance ~ grp
#> 
#>   GROUP CLD N AVG     SD  MED
#> 1   McM   a 8  29 6.1644 28.5
#> 2    MM   b 8  10 5.1270 10.5
#> 3    MS   c 8   4 3.1623  3.5
#> 4    SM   a 8  24 6.3696 23.0
#> 5    SS   b 8  11 6.7188 10.5
```

The function return a list which includes 5 components:

- method: the combination of a *a priori* and a *post-hoc* test;
- data: the input data;
- pre_hoc: An ANOVA-like table from the selected *a priori* test;
- post_hoc: pairwise comparison result from the selected *post-hoc*
  test;
- summary: descriptive statistics for reporting.

The `post_hoc` component is a data frame with at least 15 columns:

| Column name | Interpretation |
|:---|:---|
| `x1` | The name of the group being subtracted |
| `x2` | The name of the group as the subtrahend |
| `[x1 - x2]` | The difference between x1 and x2 |
| `Hedges's g` | Effect size |
| `Pvalue` | *P*-value yielded from the standard value |
| `Padj` | adjusted *P*-value according to the `p_adjust_method` argument |
| `signif` | Significance symbols. See `oneway:pval2asterisk` |
| `diff_CI` | The (1 - `alpha`) confidence interval of `[x1 - x2]` |
| `mu` | Currently not available |
| `standard_value` | The statistics of the test, generally is a symbol |
| `critical_value` | The critical value for the statistics |
| `StdErr` | The standard error used to calculate `diff_CI` |
| `method` | The name of the post-hoc method |
| `alternative` | The direction of testing, either `less`, `greater`, or `two.sided` |
| `alpha` | Error tolerance |

The `summary` component is a data frame with 13 columns:

| Column name | Interpretation |
|:---|:---|
| `GROUP` | The name of each groups from the independent variable |
| `CLD` | Compact letter display, see [`oneway::compact_letter_display`](https://p10911004-npust.github.io/oneway/reference/compact_letter_display.md) |
| `N` | Sample size |
| `AVG` | Mean |
| `SD` | Sample’s standard deviation |
| `MED` | Median |
| `MIN` | Minimum value |
| `MAX` | Maximum value |
| `CI (95%)` | Confidence interval of including the population’s mean |
| `SKEW (= 0)` | Skewness, see [`normality::skewness`](https://rdrr.io/pkg/normality/man/skewness.html) |
| `KURT (= 3)` | Kurtosis, see [`normality::kurtosis`](https://rdrr.io/pkg/normality/man/kurtosis.html) |
| `normality` | Is the sample follows normal distribution, see [`normality::is_normal`](https://rdrr.io/pkg/normality/man/is_normal.html) |
| `n_outliers` | Number of possible outliers, see [`outlying::Grubbs_test`](https://rdrr.io/pkg/outlying/man/Grubbs_test.html) |

  

#### If you only require a plug-and-play function to analyze your data, `pairwise_comparison()` has you covered. Feel free to skip the rest of this guide.

  

## 1. Interface

Most analysis functions expect a data frame and a two-sided formula:

``` r

# Pseudo code, do not run
response ~ group
```

For example:

``` r

anorexia[c(2, 19, 43, 47, 60, 71), ]
#>      therapy weight_gain
#> 2    control        -9.3
#> 19   control        -4.6
#> 43 cognitive         2.1
#> 47 cognitive        -3.7
#> 60    family        13.6
#> 71    family         5.7
```

The `anorexia` data contain `weight_gain` as the response and `therapy`
as the grouping variable:

``` r

oneway_anova(anorexia, weight_gain ~ therapy, verbose = FALSE)
#>           DF        SS       MS Fvalue  Fcrit Pvalue signif p_omega2
#> Group      2  614.6437 307.3218 5.4223 3.1296 0.0065     **   0.1094
#> Residuals 69 3910.7424  56.6774     NA     NA     NA   <NA>       NA
#> Total     71 4525.3861       NA     NA     NA     NA   <NA>       NA
#>                   method
#> Group     Fisher's ANOVA
#> Residuals Fisher's ANOVA
#> Total     Fisher's ANOVA
```

The package also accepts a list of numeric vectors for several
functions:

``` r

group_data <- list(control = rnorm(20, 10, 2),
                   treatment_A = rnorm(20, 12, 2),
                   treatment_B = rnorm(20, 15, 2))

oneway_anova(group_data)
#>           DF       SS       MS  Fvalue  Fcrit Pvalue signif p_omega2
#> Group      2 252.6691 126.3346 37.1372 3.1588      0    ***   0.5464
#> Residuals 57 193.9046   3.4018      NA     NA     NA   <NA>       NA
#> Total     59 446.5738       NA      NA     NA     NA   <NA>       NA
#>                   method
#> Group     Fisher's ANOVA
#> Residuals Fisher's ANOVA
#> Total     Fisher's ANOVA
```

  

## 2. Describe the data

[`describe()`](https://p10911004-npust.github.io/oneway/reference/describe.md)
is useful as a first-pass summary.

``` r

describe(anorexia, weight_gain ~ therapy, rounding = 2)
#>       GROUP CLD  N   AVG   SD   MED   MIN  MAX      CI (95%) SKEW (= 0)
#> 1 cognitive     29  3.01 7.31  1.40  -9.1 20.9  [0.23, 5.79]       0.93
#> 2   control     26 -0.45 7.99 -0.35 -12.2 15.9 [-3.68, 2.78]       0.37
#> 3    family     17  7.26 7.16  9.00  -5.3 21.5 [3.58, 10.94]      -0.21
#>   KURT (= 3) normality n_outliers
#> 1       3.40     FALSE          0
#> 2       2.23      TRUE          0
#> 3       2.80      TRUE          0
```

The summary includes:

| Column name | Interpretation |
|:---|:---|
| `GROUP` | The name of each groups from the independent variable |
| `CLD` | Compact letter display, see [`oneway::compact_letter_display`](https://p10911004-npust.github.io/oneway/reference/compact_letter_display.md) |
| `N` | Sample size |
| `AVG` | Mean |
| `SD` | Sample’s standard deviation |
| `MED` | Median |
| `MIN` | Minimum value |
| `MAX` | Maximum value |
| `CI (95%)` | Confidence interval of including the population’s mean |
| `SKEW (= 0)` | Skewness, see [`normality::skewness`](https://rdrr.io/pkg/normality/man/skewness.html) |
| `KURT (= 3)` | Kurtosis, see [`normality::kurtosis`](https://rdrr.io/pkg/normality/man/kurtosis.html) |
| `normality` | Is the sample follows normal distribution, see [`normality::is_normal`](https://rdrr.io/pkg/normality/man/is_normal.html) |
| `n_outliers` | Number of possible outliers, see [`outlying::Grubbs_test`](https://rdrr.io/pkg/outlying/man/Grubbs_test.html) |

Note that the `n_outliers` field provides only a **soft suggestion**
that approximately *n* observations may be outliers. It should therefore
be interpreted as a diagnostic flag rather than as evidence that those
observations should be removed.

  

## 3. *a priori* test

### 3.1 Parametric

The classic ANOVA procedure for parametric analysis is
[`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md).

``` r

anova_ooo <- oneway_anova(O_O_O, val ~ grp)
anova_ooo
#>            DF       SS      MS  Fvalue  Fcrit Pvalue signif p_omega2
#> Group       5 160.7404 32.1481 41.2416 2.2939      0    ***   0.6264
#> Residuals 114  88.8637  0.7795      NA     NA     NA   <NA>       NA
#> Total     119 249.6041      NA      NA     NA     NA   <NA>       NA
#>                   method
#> Group     Fisher's ANOVA
#> Residuals Fisher's ANOVA
#> Total     Fisher's ANOVA
```

The function checks normality and variance homogeneity. With
`var_equal = NA` (the default), it uses
[`varequal::is_var_equal`](https://rdrr.io/pkg/varequal/man/is_var_equal.html)
to check the variance homogeneity and decide to conduct:

- **Fisher’s one-way ANOVA** when equal variances are supported;
- **Welch’s ANOVA** when equal variances are not supported.

You can also force the procedure:

``` r

fisher <- oneway_anova(O_O_O, val ~ grp, var_equal = TRUE)
welch <- oneway_anova(O_X_X, val ~ grp, var_equal = FALSE)
```

Reading the ANOVA table

The Fisher/Welch’s ANOVA output includes columns such as:

- `DF`: degrees of freedom;
- `SS`: sum of squares;
- `MS`: mean square;
- `Fvalue`: observed *F* statistic;
- `Fcrit`: critical *F* value;
- `Pvalue`: omnibus *p*-value;
- `signif`: significance label;
- `p_omega2`: partial omega-squared effect size; and
- `method`: Fisher’s or Welch’s ANOVA.

The automatic selection is convenient for exploratory and routine
analyses, but it is still important to examine the study design and the
diagnostics before interpreting the result.

### 3.2 Nonparametric

#### ART-ANOVA

[`oneway_art()`](https://p10911004-npust.github.io/oneway/reference/oneway_art.md)
implements a one-way aligned-rank-transform analysis.

``` r

art <- oneway_art(X_O_O, val ~ grp)
art
#>            DF       SS         MS   Fvalue  Fcrit Pvalue signif p_omega2
#> Group       5 124695.4 24939.0800 147.3498 2.2939      0    ***   0.8591
#> Residuals 114  19294.6   169.2509       NA     NA     NA   <NA>       NA
#> Total     119 143990.0         NA       NA     NA     NA   <NA>       NA
#>              method
#> Group     ART-ANOVA
#> Residuals ART-ANOVA
#> Total     ART-ANOVA
```

Conceptually, the response is aligned with respect to the group effect,
ranked, and then analyzed with the ANOVA machinery.

``` r

attr(art, "data") |> head()
#>          y  x    residuals estimated_effect aligned_y ranked_y
#> 1 12.25383 G1  0.290503327         3.716793  4.007296      114
#> 2 11.84315 G1 -0.120171228         3.716793  3.596622      102
#> 3 12.68024 G1  0.716913640         3.716793  4.433706      116
#> 4 11.92300 G1 -0.040323232         3.716793  3.676470      103
#> 5 11.96215 G1 -0.001170422         3.716793  3.715622      106
#> 6 12.02882 G1  0.065498462         3.716793  3.782291      110
```

ART is useful when the raw response is not adequately compatible with a
classical ANOVA but a rank-based ANOVA workflow remains appropriate.

#### Kruskal–Wallis test

[`Kruskal_Wallis_test()`](https://p10911004-npust.github.io/oneway/reference/Kruskal_Wallis_test.md)
provides the classical nonparametric alternative:

``` r

kw <- Kruskal_Wallis_test(X_X_O, val ~ grp)
kw
#>            DF    SS MS       H   Hcrit Pvalue signif p_omega2         method
#> Group       5 84847 NA 70.1215 11.0705      0    ***       NA Kruskal-Wallis
#> Residuals 114    NA NA      NA      NA     NA   <NA>       NA Kruskal-Wallis
#> Total     119    NA NA      NA      NA     NA   <NA>       NA Kruskal-Wallis
```

  

## 4. Post-hoc analysis

`oneway` includes 5 multiple comparison procedures (MCP) for post-hoc
analysis. All MCPs produce a standardized result identical to the
[`pairwise_comparison()`](https://p10911004-npust.github.io/oneway/reference/pairwise_comparison.md).

### 4.1 Parametric

#### REGWQ

[`REGWQ_test()`](https://p10911004-npust.github.io/oneway/reference/REGWQ_test.md)
implements the Ryan–Einot–Gabriel–Welsch studentized-range procedure.
REGWQ is a stepwise studentized-range procedure intended primarily for
balanced, homoscedastic normal one-way designs. The package uses the
studentized-range distribution and step-specific significance levels.

``` r

regwq <- REGWQ_test(O_O_O, val ~ grp)
#> 
#> ------------------------------------
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
```

#### Tukey HSD

[`Tukey_HSD_test()`](https://p10911004-npust.github.io/oneway/reference/Tukey_HSD_test.md)
provides the classical Tukey honestly significant difference procedure.
The function is intended primarily for balanced designs with
approximately normal responses and homogeneous variances. When the
design is unbalanced, use Tukey–Kramer instead.

``` r

tukey <- Tukey_HSD_test(O_O_O, val ~ grp)
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
```

#### Tukey–Kramer

[`Tukey_Kramer_test()`](https://p10911004-npust.github.io/oneway/reference/Tukey_Kramer_test.md)
extends the Tukey’s comparison to unequal sample sizes while retaining
the equal-variance assumption:

``` r

tukey_kramer <- Tukey_Kramer_test(O_O_X, val ~ grp)
#> 
#> -------------------------------------------
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
```

#### Games–Howell

[`Games_Howell_test()`](https://p10911004-npust.github.io/oneway/reference/Games_Howell_test.md)
is intended for normal data with unequal variances and/or unequal sample
sizes. Each comparison uses a Welch–Satterthwaite-type degrees of
freedom and the studentized-range distribution. This makes Games–Howell
the natural choice among the package’s parametric post-hoc procedures
when homoscedasticity assumption is violated.

``` r

games_howell <- Games_Howell_test(O_X_X, val ~ grp)
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
```

### 4.2 Nonparametric

#### Dunn’s test

[`Dunn_test()`](https://p10911004-npust.github.io/oneway/reference/Dunn_test.md)
is the nonparametric multiple-comparison procedure associated with
Kruskal–Wallis analysis.

``` r

dunn <- Dunn_test(X_X_O, val ~ grp, p_adjust_method = "holm")
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
```

Dunn’s procedure compares mean ranks. Because many pairwise comparisons
are performed, the function supports
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) methods,
with `"holm"` as the default:

``` r

dunn_bh <- Dunn_test(X_X_O, val ~ grp, p_adjust_method = "BH")
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
#> 5    G5   b 20  9.2874 1.1111  9.1363
#> 6    G6  cd 20  7.7708 4.9047  6.9180
```

The available choices are those accepted by
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

  

## 5. `pairwise_comparison` workflow

The package’s main automatic workflow is
[`pairwise_comparison()`](https://p10911004-npust.github.io/oneway/reference/pairwise_comparison.md).

``` r

adaptive <- pairwise_comparison(O_O_O, val ~ grp)
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
```

The decision logic is:

| Data characteristics | Omnibus / pre-hoc | Post-hoc |
|----|----|----|
| Normal, equal variance, balanced | Fisher’s ANOVA | REGWQ |
| Normal, equal variance, unbalanced | Fisher’s ANOVA | Tukey–Kramer |
| Normal, unequal variance | Welch’s ANOVA | Games–Howell |
| Non-normal, ART rank response becomes suitable | ART-ANOVA | similar to normal |
| Non-normal, ART rank response remains unsuitable | Kruskal–Wallis | Dunn |

The adaptive workflow can therefore return 7 complete protocols:

1.  one-way ANOVA + REGWQ;
2.  one-way ANOVA + Tukey–Kramer;
3.  one-way ANOVA + Games–Howell;
4.  ART-ANOVA + \[REGWQ on y’’\];
5.  ART-ANOVA + \[Tukey–Kramer on y’’\];
6.  ART-ANOVA + \[Games–Howell on y’’\]; or
7.  Kruskal–Wallis + Dunn.

The Fisher’s and Welch’s ANOVA procedures are conducted on the residuals
rather than the raw response.

  

## 6. Compact letter displays

A compact letter display is a presentation device:

- groups **sharing a letter** are **NOT** significantly different;
- groups without a common letter are significantly different.

[`compact_letter_display()`](https://p10911004-npust.github.io/oneway/reference/compact_letter_display.md)
can be used independently. For example:

``` r

dunn <- Dunn_test(X_X_O, val ~ grp, verbose = FALSE)

summary_tab <- row_arrange(dunn$summary, cols = "MED")

padj_name <- "Padj (holm)"

cld <- compact_letter_display(x1 = dunn$post_hoc$x1,
                              x2 = dunn$post_hoc$x2,
                              pvalues = dunn$post_hoc[[padj_name]],
                              grp_names = summary_tab$GROUP,
                              centers = summary_tab$MED)

cld
#>   G3   G6   G2   G5   G4   G1 
#>  "d" "cd" "bc" "bc"  "b"  "a"
```

The `centers` argument controls the order used to assign letters. For
parametric procedures it is typically natural to use the mean; for
rank-based procedures, the median is more appropriate.

The symbols can also be customized:

``` r

compact_letter_display(
  x1 = dunn$post_hoc$x1,
  x2 = dunn$post_hoc$x2,
  pvalues = dunn$post_hoc[[padj_name]],
  grp_names = summary_tab$GROUP,
  centers = summary_tab$MED,
  display_letters = as.character(1:6),
  display_null_letter = "_"
)
#>      G3      G6      G2      G5      G4      G1 
#> "___4_" "__34_" "_23__" "_23__" "_2___" "1____"
```

For two-samples comparison, use
[`pval2asterisk()`](https://p10911004-npust.github.io/oneway/reference/pval2asterisk.md)
to convert numeric p-values into user-defined significance symbols.

``` r

p <- c(0.20, 0.04, 0.008, 0.0005, 1e-6)

pval2asterisk(p)
#> [1] "ns"  "*"   "**"  "***" "***"
```

The default labels are:

| Range             | Label |
|:------------------|:-----:|
| p \> 0.055        | `ns`  |
| 0.05 \< p ≤ 0.055 |  `.`  |
| 0.01 \< p ≤ 0.05  |  `*`  |
| 0.001 \< p ≤ 0.01 | `**`  |
| p ≤ 0.001         | `***` |

Custom thresholds and symbols are supported. The function expects
thresholds in descending order:

``` r

pval2asterisk(p,
              break_points = c(0.05, 0.01, 0),
              symbols = c("NS", "S", "HS"))
#> [1] "NS" "S"  "HS" "HS" "HS"
```

## 7. Effect sizes

This package provides quick inference to the effect size using only 2 of
the many estimators. For comprehensive effect size analysis, please
refer to the paper of [Lakens,
2013](https://doi.org/10.3389/fpsyg.2013.00863) and the
[effectsize](https://cran.r-project.org/package=effectsize) CRAN
package.

#### Hedges’ *g*

[`Hedges_g_s()`](https://p10911004-npust.github.io/oneway/reference/Cohen_d_s.md)
applies a small-sample correction to Cohen’s *d*:

``` r

g1 <- subset(O_O_O, grp == "G1")$val
g2 <- subset(O_O_O, grp == "G2")$val
Hedges_g_s(g1, g2)
#> Hedges' g 
#> -1.417009
```

Conventional benchmarks:

| Hedges’ *g* | Effect | Practical meaning |
|----|----|----|
| 0.2 | Small | The difference is noticeable but modest. |
| 0.5 | Medium | The difference is clearly visible. |
| 0.8 | Large | The difference is substantial and practically important. |

#### Partial-ω²

The classical ANOVA implementation reports partial omega-squared as
`p_omega2`:

``` r

fisher[, c("method", "Pvalue", "p_omega2")]
#>                   method Pvalue p_omega2
#> Group     Fisher's ANOVA      0   0.6264
#> Residuals Fisher's ANOVA     NA       NA
#> Total     Fisher's ANOVA     NA       NA
```

  

## 8. Datasets

The package includes 6 simulated datasets designed specifically for
different analysis conditions:

| Dataset | Distribution |   Variance    |  Balance   |
|:-------:|:------------:|:-------------:|:----------:|
| `O_O_O` |    Normal    |  Homogeneous  |  Balanced  |
| `O_O_X` |    Normal    |  Homogeneous  | Unbalanced |
| `O_X_X` |    Normal    | Heterogeneous | Unbalanced |
| `X_O_O` |  Non-normal  |  Homogeneous  |  Balanced  |
| `X_X_O` |  Non-normal  | Heterogeneous |  Balanced  |
| `X_X_X` |  Non-normal  | Heterogeneous | Unbalanced |

Each has six groups (`G1`–`G6`) and uses the common `grp` / `val` column
names.

Additional datasets are:

- `anorexia`: weight gain following three therapies;
- `morphine`: morphine tolerance across five treatment groups; and
- `plasma_etching`: etch rate across plasma power levels.

  

## 9. Interpretation notes

#### Statistical significance is not the same as practical importance

A small *p*-value indicates evidence against the null hypothesis under
the specified model. Effect sizes and confidence intervals provide
complementary information about magnitude and precision.

#### CLDs are a summary, not the primary inferential result

Use the pairwise table to inspect the actual differences, adjusted
*p*-values, confidence intervals, and effect sizes. Treat the CLD as a
compact visualization of those decisions.

#### Do not mechanically transform data merely to obtain a desired p-value

The package provides rank-based alternatives so that a scientifically
defensible analysis does not depend on forcing the response into a
particular distributional shape.

## Summary

A practical way to use `oneway` is:

1.  Confirm that the design is genuinely one-way: one independent
    grouping variable and one quantitative response variable.
2.  Inspect sample sizes, descriptive statistics, possible outliers, and
    distributional shape.
3.  Use
    [`oneway_anova()`](https://p10911004-npust.github.io/oneway/reference/oneway_anova.md)
    when a parametric omnibus analysis is appropriate.
4.  Use
    [`oneway_art()`](https://p10911004-npust.github.io/oneway/reference/oneway_art.md)
    when an aligned-rank-transform analysis is justified.
5.  Use
    [`Kruskal_Wallis_test()`](https://p10911004-npust.github.io/oneway/reference/Kruskal_Wallis_test.md)
    when a rank-based omnibus test is more appropriate.
6.  Choose a post-hoc procedure that matches the assumptions and balance
    of the design.
7.  Report the estimated pairwise differences, confidence intervals,
    adjusted *p*-values, and effect sizes (not only significance stars
    or CLDs).
8.  Use
    [`pairwise_comparison()`](https://p10911004-npust.github.io/oneway/reference/pairwise_comparison.md)
    when you want to formalize this decision process automatically.

The package’s automatic workflow is particularly useful when you analyze
many similar one-way experiments and want a consistent rule for
selecting the analysis protocol.

  

## References

Dinno, A. (2015). Nonparametric pairwise multiple comparisons in
independent groups using Dunn’s test. *The Stata Journal: Promoting
Communications on Statistics and Stata*, 15(1), 292–300.
<https://doi.org/10.1177/1536867X1501500117>

Elkin, L.A., Kay, M., Higgins, J.J., & Wobbrock, J.O. (2021). An aligned
rank transform procedure for multifactor contrast tests. *Proceedings of
the 34th Annual ACM Symposium on User Interface Software and
Technology*, 754–768. <https://doi.org/10.1145/3472749.3474784>

Hollander, M., Wolfe, D.A., & Chicken, E. (2014). *Nonparametric
statistical methods* (3rd ed.). Wiley.

Howell, D.C. (2010). *Statistical methods for psychology* (7th ed.).
Cengage Learning.

Howell, D.C. (2013). *Statistical methods for psychology* (8th ed.).
Cengage Learning.

Kruskal, W.H., & Wallis, W. A. (1952). Use of ranks in one-criterion
variance analysis. *Journal of the American Statistical Association*,
47(260), 583–621. <https://doi.org/10.1080/01621459.1952.10483441>

Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
cumulative science: A practical primer for t-tests and ANOVAs.
*Frontiers in Psychology*, 4(863).
<https://doi.org/10.3389/fpsyg.2013.00863>

Montgomery, D.C. (2017). *Design and analysis of experiments* (9th ed.).
John Wiley & Sons.

Piepho, H.P. (2004). An algorithm for a letter-based representation of
all-pairwise comparisons. *Journal of Computational and Graphical
Statistics*, 13(2), 456–466. <https://doi.org/10.1198/1061860043515>

Piepho, H.P. (2018). Letters in mean comparisons: What they do and don’t
mean. *Agronomy Journal*, 110(2), 431–434.
<https://doi.org/10.2134/agronj2017.10.0580>

Wobbrock, J.O., Findlater, L., Gergle, D., & Higgins, J.J. (2011). The
aligned rank transform for nonparametric factorial analyses using only
ANOVA procedures. *Proceedings of the SIGCHI Conference on Human Factors
in Computing Systems*, 143–146.
<https://doi.org/10.1145/1978942.1978963>

Zar, J.H. (2014). *Biostatistical analysis* (5th ed.). Pearson.
