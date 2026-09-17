# Convert p-values to significance labels

Converts numeric p-values into categorical significance labels according
to user-defined thresholds. The function is commonly used to annotate
statistical results in tables and figures.

## Usage

``` r
pval2asterisk(
  x,
  break_points = c(0.055, 0.05, 0.01, 0.001, 0),
  symbols = c("ns", ".", "*", "**", "***")
)
```

## Arguments

- x:

  A numeric vector of p-values.

- break_points:

  A numeric vector of significance thresholds in descending order. Each
  threshold defines the upper bound of a significance interval. The
  default values correspond to:

  - \\p \> 0.055\\: `"ns"`

  - \\0.05 \< p \le 0.055\\: `"."`

  - \\0.01 \< p \le 0.05\\: `"*"`

  - \\0.001 \< p \le 0.01\\: `"**"`

  - \\p \le 0.001\\: `"***"`

- symbols:

  A character vector of significance labels corresponding to
  `break_points`. The lengths of `break_points` and `symbols` must be
  identical.

## Value

A character vector of the same length as `x`, where each element is the
corresponding significance label.

## Details

Each p-value is assigned to exactly one interval defined by
`break_points`. Values greater than the first threshold are assigned
`symbols[1]`, whereas values less than or equal to the last threshold
are assigned the last element of `symbols`. Intermediate intervals are
matched sequentially.

The function assumes that `break_points` are supplied in descending
order.

## Examples

``` r
p <- c(0.20, 0.04, 0.008, 0.0005, 1e-6)
pval2asterisk(p)
#> [1] "ns"  "*"   "**"  "***" "***"

# Custom significance labels
pval2asterisk(
  p,
  break_points = c(0.05, 0.01, 0),
  symbols = c("Not significant", "Significant", "Highly significant")
)
#> [1] "Not significant"    "Significant"        "Highly significant"
#> [4] "Highly significant" "Highly significant"
```
