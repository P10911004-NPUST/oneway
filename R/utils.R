#' Check Balance of Sample Sizes Among Groups
#'
#' Determines whether sample sizes are approximately balanced across groups.
#' Sample sizes are considered balanced when the smallest and largest group
#' sizes are within the specified tolerance range relative to the average
#' sample size.
#'
#' @param data A data frame or a list containing observations from each group.
#' @param formula A formula specifying the dependent variable (DV) and the
#'        independent variable (IV) in the form `DV ~ IV`. This argument is required
#'        when `data` is a data frame and is ignored when `data` is already a list.
#' @param buffer_ratio Numeric value between 0 and 1 (default: 0.2). The allowable
#'        proportional deviation from the mean sample size.
#'        For example, `buffer_ratio = 0.2` allows group sizes to differ from the
#'        mean by up to 20%. When `buffer_ratio = 0`, the function requires exact
#'        equality of sample sizes among groups.
#'
#' @returns
#' A logical value:
#' \describe{
#'   \item{TRUE}{Sample sizes among groups are considered balanced.}
#'   \item{FALSE}{At least one group has a sample size outside the specified
#'   tolerance range.}
#' }
#'
#' @details
#' The function compares each group's sample size with the mean sample size
#' across groups. The sample sizes are considered balanced if:
#'
#' \deqn{
#' (1-r) \leq \frac{\min(n)}{\bar{n}} \leq
#' \frac{\max(n)}{\bar{n}} \leq (1+r)
#' }
#'
#' where \eqn{\bar{n}} is the mean sample size across groups and `r` is `buffer_ratio`.
#'
#' @examples
#' is_balance(list(rnorm(10), rnorm(13)), buffer_ratio = 0.2)
#' is_balance(list(rnorm(10), rnorm(13)), buffer_ratio = 0.1)
#' @export
is_balance <- function(data, formula, buffer_ratio = 0.2)
{
    lst <- tidy_to_list(data, formula)
    n <- unlist(lapply(lst, length), use.names = FALSE)
    if (length(unique(n)) == 1)
        return(TRUE)
    a <- (max(n) / mean(n)) <= (1 + buffer_ratio)
    b <- (min(n) / mean(n)) >= (1 - buffer_ratio)
    return(a & b)
}


#' Descriptive statistics
#'
#' Compute and summarize descriptive statistics for one or more groups.
#'
#' @param data A data frame or a list.
#' @param formula A formula with a dependent variable (DV) and an independent variable (IV).
#'        For example: `DV ~ IV`.
#' @param rounding Integer (default: 2). Rounding digits.
#'
#' @returns
#' A data frame with 13 columns:
#' \describe{
#'   \item{GROUP}{Group name.}
#'   \item{CLD}{Compact letter display for multiple comparisons. This column is
#'   returned as an empty character vector and is intended to be filled by
#'   post hoc comparison functions. See `oneway::compact_letter_display`.}
#'   \item{N}{Sample size.}
#'   \item{AVG}{Arithmetic mean.}
#'   \item{SD}{Sample standard deviation.}
#'   \item{MED}{Median.}
#'   \item{MIN}{Minimum observed value.}
#'   \item{MAX}{Maximum observed value.}
#'   \item{CI (95%)}{Two-sided 95% confidence interval for the population mean.}
#'   \item{SKEW (= 0)}{Normal distribution has a skewness of 0. See `normality::skewness`.}
#'   \item{KURT (= 3)}{Normal distribution has a kurtosis of 3. See `normality::kurtosis`.}
#'   \item{normality}{Is the data normally distributed? See `normality::is_normal`.}
#'   \item{n_outliers}{Number of possible outliers. See `outlying::Grubbs_test`.}
#' }
#'
#' @examples
#' y1 <- c(stats::rnorm(20), 7)
#' y2 <- c(stats::rnorm(22), -7, 9)
#' describe(list("apple" = y1, "banana" = y2))
#' @export
describe <- function(data, formula, rounding = 2)
{
    lst <- tidy_to_list(data, formula)
    n_grps <- length(lst)
    grp_names <- names(lst)

    .skew <- function(x)
    {
        skew <- normality::skewness(x, silent = TRUE)[["statistic"]][["G1"]]
        round(skew, rounding)
    }

    .kurt <- function(x)
    {
        kurt <- normality::kurtosis(x, silent = TRUE)[["statistic"]][["G2"]]
        round(kurt, rounding)
    }

    .outliers <- function(x)
    {
        suppressWarnings(
            out <- outlying::Grubbs_test(x)
        )
        n_out <- sum(unname(out))
        return(n_out)
    }

    CI_lower <- unlist(lapply(lst, function(x) CI_pop_mean(x)[[1]]), use.names = FALSE)
    CI_upper <- unlist(lapply(lst, function(x) CI_pop_mean(x)[[2]]), use.names = FALSE)
    confidence_interval <- sprintf("[%s, %s]",
                                   round(CI_lower, rounding),
                                   round(CI_upper, rounding))

    df0 <- data.frame(
        row.names      = NULL,
        check.names    = FALSE,
        "GROUP"        = grp_names,
        "CLD"          = character(n_grps),
        "N"            = unlist(lapply(lst, length), use.names = FALSE),
        "AVG"          = round(unlist(lapply(lst, mean), use.names = FALSE), rounding),
        "SD"           = round(unlist(lapply(lst, stats::sd), use.names = FALSE), rounding),
        "MED"          = round(unlist(lapply(lst, stats::median), use.names = FALSE), rounding),
        "MIN"          = round(unlist(lapply(lst, min), use.names = FALSE), rounding),
        "MAX"          = round(unlist(lapply(lst, max), use.names = FALSE), rounding),
        "CI (95%)"     = confidence_interval,
        "SKEW (= 0)"   = unlist(lapply(lst, .skew), use.names = FALSE),
        "KURT (= 3)"   = unlist(lapply(lst, .kurt), use.names = FALSE),
        "normality"    = unlist(lapply(lst, normality::is_normal), use.names = FALSE),
        "n_outliers"   = unlist(lapply(lst, .outliers), use.names = FALSE)
    )

    return(df0)
}


function_to_character <- function(func)
{
    if (!is.function(func))
        stop("`func` must be a function.")

    env <- environmentName(environment(func))

    if (env == "R_GlobalEnv")
    {
        pkg <- NULL
        fn <- deparse(substitute(func))
        ret <- fn
    } else {
        pkg <- env
        fns <- getNamespaceExports(env)
        ind <- vapply(X = fns,
                      function(fn)
                      {
                          fn <- getExportedValue(pkg, fn)
                          identical(fn, func)
                      },
                      FUN.VALUE = logical(1))
        ret <- paste(pkg, fns[ind], sep = "::")
    }

    return(ret)
}

