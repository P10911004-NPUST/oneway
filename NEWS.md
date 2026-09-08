# oneway 0.0.2

- make the package description more concise and direct;
- added several relevant references to the DESCRIPTION file;
- changed all `silent` argument to `verbose`;
- enclosed the `print()` and `cat()` calls within `if (verbose) { ... }` blocks.

## Reviewer's comments (first revision for v0.0.1)

#### 1. Please do not start the DESCRIPTION with "An integrated toolkit".

The DESCRIPTION has been revised to make the package description more concise and direct.

#### 2. Referencing in DESCRIPTION

I have added several relevant references to the DESCRIPTION file to provide appropriate guidance and context for the methods implemented in the package.

#### 3. You write information messages to the console that cannot be easily suppressed.

I have enclosed the `print()` and `cat()` calls within `if (verbose) { ... }` blocks so that informational messages can be easily suppressed by setting `verbose = FALSE`.

# oneway 0.0.1

* Initial CRAN submission.
