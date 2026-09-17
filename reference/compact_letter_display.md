# Compact Letter Display (CLD)

Represent significance statements resulting from all-pairwise
comparisons.

## Usage

``` r
compact_letter_display(
  x1,
  x2,
  pvalues,
  grp_names,
  centers,
  alpha = 0.05,
  descending = TRUE,
  display_letters = base::letters,
  display_null_letter = "",
  misc = FALSE
)
```

## Arguments

- x1:

  Character vector. The names of the minuend.

- x2:

  Character vector. The names of the subtrahend.

- pvalues:

  Numeric vector. The p-values for the differences of each `x1 - x2`.

- grp_names:

  Character vector. The group names (each factor levels).

- centers:

  Numeric vector. Generally, the corresponding mean or median values of
  `grp_names`.

- alpha:

  Numeric (default: 0.05). Significance level, range from 0 to 1.

- descending:

  Logical (default: TRUE). If `TRUE`, sort the centers in decreasing
  order.

- display_letters:

  Character vector (default:
  [`base::letters`](https://rdrr.io/r/base/Constants.html)). Display
  symbols.

- display_null_letter:

  Character (default: ""). Symbol for filling the letter's gap.

- misc:

  Logical (default: FALSE). Return other unimportant variables, not for
  users.

## Value

A character vector.

## References

Piepho, H.-P. (2004). An algorithm for a letter-based representation of
all-pairwise comparisons. Journal of Computational and Graphical
Statistics, 13(2), 456–466. https://doi.org/10.1198/1061860043515

Piepho, H.-P. (2018). Letters in mean comparisons: What they do and
don't mean. Agronomy Journal, 110(2), 431–434.
https://doi.org/10.2134/agronj2017.10.0580

## Examples

``` r
out <- Dunn_test(X_X_O, val ~ grp, verbose = FALSE)
tab <- row_arrange(out[["summary"]], "MED")
post <- out[["post_hoc"]]
print(post[, 1:7])
#>    x1 x2 [x1 - x2] Hedges's g Pvalue Padj (holm) signif
#> 1  G1 G2     50.20     2.0229 0.0000      0.0000    ***
#> 2  G1 G3     84.15     9.5125 0.0000      0.0000    ***
#> 3  G1 G4     30.60     2.0015 0.0054      0.0378      *
#> 4  G1 G5     36.60     2.4555 0.0009      0.0090     **
#> 5  G1 G6     64.55     2.4937 0.0000      0.0000    ***
#> 6  G2 G3     33.95     1.3928 0.0020      0.0180      *
#> 7  G2 G4    -19.60    -0.7159 0.0748      0.3740     ns
#> 8  G2 G5    -13.60    -0.5006 0.2163      0.5760     ns
#> 9  G2 G6     14.35     0.4167 0.1920      0.5760     ns
#> 10 G3 G4    -53.55    -3.6776 0.0000      0.0000    ***
#> 11 G3 G5    -47.55    -3.3584 0.0000      0.0000    ***
#> 12 G3 G6    -19.60    -0.7697 0.0748      0.3740     ns
#> 13 G4 G5      6.00     0.3180 0.5854      0.5854     ns
#> 14 G4 G6     33.95     1.1974 0.0020      0.0180      *
#> 15 G5 G6     27.95     0.9930 0.0111      0.0666     ns
cld <- compact_letter_display(x1 = post$x1,
                              x2 = post$x2,
                              pvalues = post$`Padj (holm)`,
                              grp_names = tab$GROUP,
                              centers = tab$MED)
print(cld)
#>   G3   G6   G2   G5   G4   G1 
#>  "d" "cd" "bc" "bc"  "b"  "a" 
```
