# Generate All Pairwise Contrasts

Generate a contrast matrix containing all pairwise comparisons among
groups. Each row represents one pairwise contrast, with a coefficient of
`1` assigned to the first group and `-1` assigned to the second group.
All other coefficients are `0`. The sum of all coefficients is zero.

## Usage

``` r
pairwise_contrasts(group_names)
```

## Arguments

- group_names:

  A character vector of group names or group labels.

## Value

A numeric matrix with one row for each pairwise comparison and one
column for each unique group. Column names correspond to the group
names. Each row contains one coefficient of `1`, one coefficient of
`-1`, and zeros elsewhere.

## Details

The contrasts are generated in the order returned by
[`utils::combn()`](https://rdrr.io/r/utils/combn.html), with the first
group in each pair assigned a coefficient of `1` and the second group
assigned a coefficient of `-1`.

The resulting matrix can be used to specify all-pairwise comparisons in
multiple-comparison procedures, such as Scheffe's test.

## See also

[`utils::combn()`](https://rdrr.io/r/utils/combn.html)

## Examples

``` r
groups <- c("g1", "g2", "g3", "g4", "g5")
pairwise_contrasts(groups)
#>       g1 g2 g3 g4 g5
#>  [1,]  1 -1  0  0  0
#>  [2,]  1  0 -1  0  0
#>  [3,]  1  0  0 -1  0
#>  [4,]  1  0  0  0 -1
#>  [5,]  0  1 -1  0  0
#>  [6,]  0  1  0 -1  0
#>  [7,]  0  1  0  0 -1
#>  [8,]  0  0  1 -1  0
#>  [9,]  0  0  1  0 -1
#> [10,]  0  0  0  1 -1

# Use the unique group names from a grouping vector
group_names <- c("Control", "Low", "High", "Control", "Low")
pairwise_contrasts(group_names)
#>      Control Low High
#> [1,]       1  -1    0
#> [2,]       1   0   -1
#> [3,]       0   1   -1
```
