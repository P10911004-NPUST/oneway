# Effect size

Calculate Cohen's d and Hedges' g introduced in Lakens (2013).

## Usage

``` r
Cohen_d_s(
  y1,
  y2,
  alternative = "two.sided",
  alpha = 0.05,
  mu = 0,
  return_CI = FALSE
)

Hedges_g_s(
  y1,
  y2,
  alternative = "two.sided",
  alpha = 0.05,
  mu = 0,
  return_CI = FALSE
)
```

## Arguments

- y1:

  A numeric vector.

- y2:

  A numeric vector.

- alternative:

  Character (default: `"two.sided"`). Specifies the alternative
  hypothesis. Available options are `c("two.sided", "less", "greater")`.

- alpha:

  Numeric (default: 0.05). Significance level (0 - 1) for hypothesis
  testing.

- mu:

  Numeric (default: 0).

- return_CI:

  Logical (default: FALSE). Whether to return the confidence interval of
  the effect size.

## Value

A numeric scalar or a numeric vector of length 3, depends on
`return_CI`.

## Details

Refer to Lakens (2013), the Cohen's d is the formula 1, the Hedges' g is
the formula 4. Their confidence interval (CI) were calculated based on
formula 2, by transforming the `d` and `g` back to `t` to fit the
general confidence interval calculation. For detailed and precise CI
estimates, please use the `effectsize` package.

## References

Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
cumulative science: A practical primer for t-tests and ANOVAs. Frontiers
in Psychology, 4. https://doi.org/10.3389/fpsyg.2013.00863
