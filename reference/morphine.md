# Morphine tolerance data

A dataset containing morphine tolerance measurements from five
experimental treatment groups. The data consist of tolerance values
recorded for eight experimental units in each group, resulting in a
balanced one-way design with 40 observations.

## Usage

``` r
morphine
```

## Format

A data frame with 40 rows and 2 columns:

- tolerance:

  A numeric variable containing the morphine tolerance measurement.

- grp:

  A factor identifying the experimental treatment group with five
  levels: `"MS"`, `"MM"`, `"SS"`, `"SM"`, and `"McM"`.

## Details

The treatment groups are coded as:

- MS:

  Morphine followed by saline.

- MM:

  Morphine followed by morphine.

- SS:

  Saline followed by saline.

- SM:

  Saline followed by morphine.

- McM:

  Morphine followed by challenge morphine.

The dataset represents a balanced one-way experimental design with five
independent groups and eight observations per group. It is suitable for
demonstrating one-way ANOVA, nonparametric alternatives, homogeneity of
variance tests, post hoc comparisons, and effect size estimation.

## References

Howell, D. C. (2013). Statistical methods for psychology (8th ed.).
Cengage. Chapter 11, Table 12.1, pg. 375.
