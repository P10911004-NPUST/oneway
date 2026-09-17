# Check Balance of Sample Sizes Among Groups

Determines whether sample sizes are approximately balanced across
groups. Sample sizes are considered balanced when the smallest and
largest group sizes are within the specified tolerance range relative to
the average sample size.

## Usage

``` r
is_balance(data, formula, buffer_ratio = 0)
```

## Arguments

- data:

  A data frame or a list containing observations from each group.

- formula:

  A formula specifying the dependent variable (DV) and the independent
  variable (IV) in the form `DV ~ IV`. This argument is required when
  `data` is a data frame and is ignored when `data` is already a list.

- buffer_ratio:

  Numeric value between 0 and 1 (default: 0). The allowable proportional
  deviation from the mean sample size. For example, `buffer_ratio = 0.2`
  allows group sizes to differ from the mean by up to 20%. When
  `buffer_ratio = 0`, the function requires exact equality of sample
  sizes among groups.

## Value

A logical value:

- TRUE:

  Sample sizes among groups are considered balanced.

- FALSE:

  At least one group has a sample size outside the specified tolerance
  range.

## Details

The function compares each group's sample size with the mean sample size
across groups. The sample sizes are considered balanced if:

\$\$ (1-r) \leq \frac{\min(n)}{\bar{n}} \leq \frac{\max(n)}{\bar{n}}
\leq (1+r) \$\$

where \\\bar{n}\\ is the mean sample size across groups and `r` is
`buffer_ratio`.

## Examples

``` r
is_balance(list(rnorm(10), rnorm(13)), buffer_ratio = 0.2)
#> [1] TRUE
is_balance(list(rnorm(10), rnorm(13)), buffer_ratio = 0.1)
#> [1] FALSE
```
