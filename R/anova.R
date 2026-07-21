#' One-Way Aligned Ranked Transformed Analysis of Variance (ART-ANOVA)
#'
#' Performs a one-way analysis of variance (ANOVA) to compare the means of two
#' or more independent groups. By default, the function automatically selects
#' between the classical Fisher ANOVA and Welch's ANOVA according to whether
#' the group variances are judged to be homogeneous.
#'
#' If `var_equal = TRUE`, Fisher's ANOVA is performed on the transformed response variable.
#' If `var_equal = FALSE`, Welch's ANOVA is performed. When `var_equal = NA` (the default),
#' homogeneity of variances is assessed using [varequal::is_var_equal()], and the appropriate
#' test is selected automatically.
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
#' @param factor_levels Specify the order of the factor levels (default: NULL).
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
#'   \item{method}{A character specifying this is an ART-ANOVA procedure.}
#' }
#'
#' The rows correspond to the treatment groups ("Group"), residual error
#' ("Residuals"), and total variation ("Total").
#'
#' @references
#' Wobbrock, J. O., Findlater, L., Gergle, D., & Higgins, J. J. (2011).
#' The aligned rank transform for nonparametric factorial analyses using only ANOVA procedures.
#' Proceedings of the SIGCHI Conference on Human Factors in Computing Systems, 2011, 143–146.
#' https://doi.org/10.1145/1978942.1978963
#'
#' Elkin, L. A., Kay, M., Higgins, J. J., & Wobbrock, J. O. (2021).
#' An aligned rank transform procedure for multifactor contrast tests.
#' Proceedings of the 34th Annual ACM Symposium on User Interface Software and Technology, 754–768.
#' https://doi.org/10.1145/3472749.3474784
#'
#' @examples
#' oneway_art(anorexia, weight_gain ~ therapy)
#'
#' @export
oneway_art <- function(
        data,
        formula,
        alpha = 0.05,
        var_equal = NA,
        factor_levels = NULL
) {

    #----------------------------------------------------------------------------------------------#
    #                             Aligned Ranked Transform (ART)
    #----------------------------------------------------------------------------------------------#
    df0 <- tidy_to_dataframe(data, formula, factor_levels)

    aov_mod <- stats::aov(y ~ x, df0)
    df0[["residuals"]] <- stats::residuals(aov_mod)

    yij <- df0[["y"]]
    y_bar <- mean(yij)
    estimated_effect <- tapply(yij, df0[["x"]], function(x) mean(x) - y_bar)
    df0[["estimated_effect"]] <- vapply(X = df0[["x"]],
                                        function(x)
                                        {
                                            ind <- match(x, names(estimated_effect))
                                            return(estimated_effect[[ind]])
                                        },
                                        FUN.VALUE = numeric(1))

    rounding <- abs(floor(log10(.Machine$double.eps)))
    df0[["aligned_y"]] <- round(df0[["residuals"]] + df0[["estimated_effect"]], rounding)
    df0[["ranked_y"]] <- rank(df0[["aligned_y"]])

    #----------------------------------------------------------------------------------------------#
    #                                    ANOVA
    #----------------------------------------------------------------------------------------------#
    aov_tab <- oneway_anova(df0, ranked_y ~ x, alpha = alpha)
    aov_tab[["method"]] <- "ART-ANOVA"

    df0 <- tidy_to_dataframe(data, formula)

    structure(
        aov_tab,
        "data" = df0,
        class = c("oneway.anova_table", "data.frame")
    )
}


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
#' @param factor_levels Specify the order of the factor levels (default: NULL).
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
#'
#' @export
oneway_anova <- function(
        data,
        formula,
        alpha = 0.05,
        var_equal = NA,
        factor_levels = NULL
) {
    lst <- tidy_to_list(data, formula, factor_levels)

    if (isTRUE(var_equal) || isFALSE(var_equal))
        is_var_equal <- var_equal
    else
        is_var_equal <- varequal::is_var_equal(lst)

    if (isTRUE(is_var_equal))
        aov_tab <- .fisher_anova(lst, alpha = alpha)
    else
        aov_tab <- .welch_anova(lst, alpha = alpha)

    df0 <- tidy_to_dataframe(data, formula, factor_levels)

    structure(
        aov_tab,
        "data" = df0,
        class = c("oneway.anova_table", "data.frame")
    )
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
    n <- unlist(lapply(lst, length), use.names = FALSE)  # Each group sample sizes
    yi_bar <- unlist(lapply(lst, mean), use.names = FALSE)  # Each group means

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

