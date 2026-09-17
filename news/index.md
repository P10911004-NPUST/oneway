# Changelog

## oneway 0.0.3

## oneway 0.0.2

CRAN release: 2026-09-15

- make the package description more concise and direct;
- added several relevant references to the DESCRIPTION file;
- changed all `silent` argument to `verbose`;
- enclosed the [`print()`](https://rdrr.io/r/base/print.html) and
  [`cat()`](https://rdrr.io/r/base/cat.html) calls within
  `if (verbose) { ... }` blocks.

#### Reviewer’s comments (first revision for v0.0.1)

##### 1. Please do not start the DESCRIPTION with “An integrated toolkit…”.

The DESCRIPTION has been revised to make the package description more
concise and direct.

##### 2. Referencing in DESCRIPTION

I have added several relevant references to the DESCRIPTION file to
provide appropriate guidance and context for the methods implemented in
the package.

Refer to [The CRAN Cookbook (description issues
\#references)](https://contributor.r-project.org/cran-cookbook/description_issues.html#references)

##### 3. You write information messages to the console that cannot be easily suppressed.

I have enclosed the [`print()`](https://rdrr.io/r/base/print.html) and
[`cat()`](https://rdrr.io/r/base/cat.html) calls within
`if (verbose) { ... }` blocks so that informational messages can be
easily suppressed by setting `verbose = FALSE`.

Refer to [The CRAN Cookbook (code issues
\#using-printcat)](https://contributor.r-project.org/cran-cookbook/code_issues.html#using-printcat)

## oneway 0.0.1

- Initial CRAN submission.
