## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This is the first revision.

## Reviewer's comments (first revision)

Many thanks to the reviewer for taking the time to review the package and for providing helpful and constructive suggestions.

#### 1. Please do not start the DESCRIPTION with "An integrated toolkit".

The DESCRIPTION has been revised to make the package description more concise and direct.

#### 2. Referencing in DESCRIPTION

I have added several relevant references to the DESCRIPTION file to provide appropriate guidance and context for the methods implemented in the package.

#### 3. You write information messages to the console that cannot be easily suppressed.

I have enclosed the `print()` and `cat()` calls within `if (verbose) { ... }` blocks so that informational messages can be easily suppressed by setting `verbose = FALSE`.
