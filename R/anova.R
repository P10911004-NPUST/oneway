#' One-Way Analysis of Variance
#'
#' Performs a one-way analysis of variance (ANOVA) to compare the means of two
#' or more independent groups. By default, the function automatically selects
#' between the classical Fisher ANOVA and Welch's ANOVA according to whether
#' the group variances are judged to be homogeneous.
#'
#' If `var_equal = TRUE`, Fisher's ANOVA is performed. If
#' `var_equal = FALSE`, Welch's ANOVA is performed. When
#' `var_equal = NA` (the default), homogeneity of variances is assessed using
#' [varequal::is_var_equal()], and the appropriate test is selected
#' automatically.
#'
#' @param data A data frame containing the response and grouping variables.
#' @param formula A two-sided formula specifying the response and grouping
#'   variables in the form `response ~ group`.
#' @param alpha A numeric significance level used to compute the critical
#'   F-value. The default is `0.05`.
#' @param var_equal A logical value indicating whether equal variances should be
#'   assumed. If `TRUE`, Fisher's ANOVA is performed. If `FALSE`, Welch's ANOVA
#'   is performed. If `NA` (default), equality of variances is determined
#'   automatically using [varequal::is_var_equal()].
#'
#' @return
#' A data frame representing the ANOVA table with the following columns:
#' \describe{
#'   \item{DF}{Degrees of freedom.}
#'   \item{SS}{Sum of squares.}
#'   \item{MS}{Mean square.}
#'   \item{Fvalue}{Observed F statistic.}
#'   \item{Fcrit}{Critical F value at the specified significance level.}
#'   \item{Pvalue}{P-value associated with the F statistic.}
#'   \item{signif}{Significance code corresponding to the p-value.}
#'   \item{method}{Show whether Fisher's or Welch's ANOVA was conducted.}
#' }
#'
#' The rows correspond to the treatment groups ("Group"), residual error
#' ("Residuals"), and total variation ("Total").
#'
#' @details
#' Fisher's ANOVA assumes independent observations, normally distributed
#' populations, and equal population variances. When the equal-variance
#' assumption is violated, Welch's ANOVA provides a more robust alternative by
#' adjusting the test statistic and denominator degrees of freedom.
#'
#' @references
#' Howell, D. C. (2013). Statistical methods for psychology (Eighth edition). Cengage.
#'
#' @examples
#' # Automatically select the appropriate procedure
#' oneway_anova(anorexia, weight_gain ~ therapy)
#'
#' # Classical one-way ANOVA
#' oneway_anova(plasma_etching, etch_rate ~ power, var_equal = TRUE)
#'
#' # Welch's ANOVA
#' oneway_anova(anorexia, weight_gain ~ therapy, var_equal = FALSE)
#' @export
oneway_anova <- function(
        data,
        formula,
        alpha = 0.05,
        var_equal = NA
) {
    lst <- tidy_to_list(data, formula)

    if (isTRUE(var_equal) || isFALSE(var_equal))
        is_var_equal <- var_equal
    else
        is_var_equal <- varequal::is_var_equal(lst)

    if (isTRUE(is_var_equal))
        aov_tab <- .fisher_anova(lst, alpha = alpha)
    else
        aov_tab <- .welch_anova(lst, alpha = alpha)

    return(aov_tab)
}


#--------------------------------------------------------------------------------------------------#
#                                        Internal function                                         #
#--------------------------------------------------------------------------------------------------#
.fisher_anova <- function(
        data,
        formula,
        alpha = 0.05
) {
    lst <- tidy_to_list(data, formula)

    yij <- unlist(lst, use.names = FALSE)  # All observations
    yi <- unlist(lapply(lst, sum), use.names = FALSE)  # Sum of each groups
    k <- length(lst)  # group numbers
    N <- length(yij)  # Total sample size
    y_bar <- mean(yij)  # Grand mean
    n <- unlist(lapply(lst, length), use.names = FALSE)  # Each group sample size
    yi_bar <- unlist(lapply(lst, mean), use.names = FALSE)  # Each group mean

    DF_between <- k - 1
    DF_within <- N - k
    DF_total <- N - 1

    SS_total <- sum((yij - y_bar) ^ 2)
    SS_between <- sum(n * ((yi_bar - y_bar) ^ 2))
    # SS_within <- sum(unlist(lapply(lst, ss), use.names = FALSE))
    SS_within <- SS_total - SS_between

    MS_between <- SS_between / DF_between
    MS_within <- SS_within / DF_within  # MSE: Mean Square Error

    Fval <- MS_between / MS_within
    Fval_crit <- stats::qf(alpha, DF_between, DF_within, lower.tail = FALSE)
    pval <- stats::pf(Fval, DF_between, DF_within, lower.tail = FALSE)

    asterisk <- pval2asterisk(pval, break_points = c(alpha + 0.005, alpha, 0.01, 0.001, 0))

    aov_tab <- data.frame(
        row.names   = c("Group", "Residuals", "Total"),
        "DF"        = c(DF_between, DF_within, DF_total),
        "SS"        = c(SS_between, SS_within, SS_total),
        "MS"        = c(MS_between, MS_within, NA_real_),
        "Fvalue"    = c(Fval, NA_real_, NA_real_),
        "Fcrit"     = c(Fval_crit, NA_real_, NA_real_),
        "Pvalue"    = c(pval, NA_real_, NA_real_),
        "signif"    = c(asterisk, NA_character_, NA_character_),
        "method"    = "Fisher's ANOVA"
    )

    return(aov_tab)
}


.welch_anova <- function(
        data,
        formula,
        alpha = 0.05
) {
    lst <- tidy_to_list(data, formula)

    yij <- unlist(lst, use.names = FALSE)  # All observations
    yi <- unlist(lapply(lst, sum), use.names = FALSE)  # Sum of each groups
    k <- length(lst)  # group numbers
    N <- length(yij)  # Total sample size
    y_bar <- mean(yij)  # Grand mean
    n <- unlist(lapply(lst, length), use.names = FALSE)  # Each group sample size

    wk <- n / unlist(lapply(lst, stats::var), use.names = FALSE)

    yi_bar <- unlist(lapply(lst, mean), use.names = FALSE)  # Each group mean
    yi_bar_bar <- sum(wk * yi_bar) / sum(wk)
    denom_block <- sum((1 / (n - 1)) * (1 - wk / sum(wk)) ^ 2)

    DF_between <- k - 1
    DF_within <- (k * k - 1) / (3 * denom_block)
    DF_total <- DF_between + DF_within

    SS_between <- sum(wk * (yi_bar - yi_bar_bar) ^ 2)
    SS_within <- DF_within * (1 + (2 * (k - 2) / (k * k - 1)) * denom_block)
    SS_total <- sum((yij - y_bar) ^ 2)

    MS_between <- SS_between / DF_between
    MS_within <- SS_within / DF_within  # MSE: Mean Square Error

    Fval <- MS_between / MS_within
    Fval_crit <- stats::qf(alpha, DF_between, DF_within, lower.tail = FALSE)
    pval <- stats::pf(Fval, DF_between, DF_within, lower.tail = FALSE)

    asterisk <- pval2asterisk(pval, break_points = c(alpha + 0.005, alpha, 0.01, 0.001, 0))

    aov_tab <- data.frame(
        row.names   = c("Group", "Residuals", "Total"),
        "DF"        = c(DF_between, DF_within, DF_total),
        "SS"        = c(SS_between, SS_within, SS_total),
        "MS"        = c(MS_between, MS_within, NA_real_),
        "Fvalue"    = c(Fval, NA_real_, NA_real_),
        "Fcrit"     = c(Fval_crit, NA_real_, NA_real_),
        "Pvalue"    = c(pval, NA_real_, NA_real_),
        "signif"    = c(asterisk, NA_character_, NA_character_),
        "method"    = "Welch's ANOVA"
    )

    return(aov_tab)
}

