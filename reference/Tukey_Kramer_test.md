# Tukey-Kramer test

Represent significance statements resulting from all-pairwise
comparisons.

## Usage

``` r
Tukey_Kramer_test(
  data,
  formula = NULL,
  alpha = 0.05,
  rounding = 4,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame in which the variables specified in the formula will be
  found.

- formula:

  A formula specifying the model.

- alpha:

  Numeric value range from 0 to 1 (default: 0.05). The error tolerance.

- rounding:

  Integer (default: 4). Rounding digits.

- verbose:

  Logical (default: TRUE). Show warnings and messages.

## Value

A list with 4 elements:

- method:

  Statistical procedures that were conducted.

- data:

  The input data and possibly other transformed data.

- pre_hoc:

  *a priori* test result.

- post_hoc:

  Post-hoc test result.

- summary:

  Descriptive statistics.

## References

Howell, D. C. (2013). Statistical Methods for Psychology (8th ed.).
Cengage Learning. Chapter 12: Multiple comparisons among treatment
means, Section 12.7: Tukey's test, pg. 394.

Zar, J. H. (2014). Biostatistical analysis (5th edition). Pearson.
Chapter 11: Multiple comparisons, pg. 244-245.

## Examples

``` r
out <- Tukey_Kramer_test(O_O_X, val ~ grp)
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
#> 
```
