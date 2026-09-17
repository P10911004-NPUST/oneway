# oneway

An R package for one-way statistical analyses.

[Get
started](https://p10911004-npust.github.io/oneway/articles/oneway.html)

There are also other nice alternatives such as
[`agricolae`](https://cran.r-project.org/package=agricolae),
[`car`](https://cran.r-project.org/package=car),
[`onewaytests`](https://cran.r-project.org/package=onewaytests), and
other friends.

# Installation

You can install the package from
[CRAN](https://cran.r-project.org/package=oneway) with:

``` r

install.packages("oneway")
```

or the development version from
[GitHub](https://github.com/P10911004-NPUST/oneway) with:

``` r

if (!require("pak")) install.packages("pak")
pak::pak("P10911004-NPUST/oneway")
```

# Quick start

``` r

pairwise_comparison(O_O_O, val ~ grp)
```
