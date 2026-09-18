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

# TODO

pairwise *t*-test

data transformation (maybe not necessary)

Scheffé test

- [Scheffé, H. (1953)](https://doi.org/10.1093/biomet/40.1-2.87)

Dunnett test

- [Dunnett, C.W. (1955)](https://doi.org/10.1080/01621459.1955.10501294)
