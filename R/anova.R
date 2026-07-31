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
#' @param rounding Integer (default: 4). Rounding digits.
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
        rounding = 4
) {

    # -------------------------------------------------------------------------------------------- #
    #                             Aligned Ranked Transform (ART)
    # -------------------------------------------------------------------------------------------- #
    df0 <- tidy_to_dataframe(data, formula)

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

    digits <- abs(floor(log10(.Machine$double.eps)))
    df0[["aligned_y"]] <- round(df0[["residuals"]] + df0[["estimated_effect"]], digits)
    df0[["ranked_y"]] <- rank(df0[["aligned_y"]])

    # -------------------------------------------------------------------------------------------- #
    #                                    ANOVA
    # -------------------------------------------------------------------------------------------- #
    aov_tab <- oneway_anova(df0, ranked_y ~ x, alpha, rounding = rounding)
    aov_tab[["method"]] <- "ART-ANOVA"

    structure(
        aov_tab,
        "data" = df0,
        class = c("oneway.anova_table", "oneway.art", "data.frame")
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
#' @param rounding Integer (default: 4). Rounding digits.
#' @param silent Logical (default: FALSE). Suppress warnings and messages.
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
#'   \item{p_omega2}{Effect size. Partial omega squared.}
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
#' Howell, D. C. (2013). Statistical methods for psychology (8th edition).
#' Cengage. Chapter 11, pg. 325-345.
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
        rounding = 4,
        silent = FALSE
) {
    lst <- tidy_to_list(data, formula)

    # ------------------------------------------------------------------------------------- #
    #                                 Check normality                                       #
    # ------------------------------------------------------------------------------------- #
    # is_normal <- normality::is_normal(lst)
    # if (isFALSE(is_normal) & isFALSE(silent))
    #     warning("Normality assumption is violated. Consider ART-ANOVA or Kruskal-Wallis.")

    # ------------------------------------------------------------------------------------- #
    #                              Check homoscedasticity                                   #
    # ------------------------------------------------------------------------------------- #
    if (isTRUE(var_equal) || isFALSE(var_equal))
        is_var_equal <- var_equal
    else
        is_var_equal <- varequal::is_var_equal(lst)

    if (isTRUE(is_var_equal))
        aov_tab <- .fisher_anova(lst, alpha = alpha, rounding = rounding)
    else
        aov_tab <- .welch_anova(lst, alpha = alpha, rounding = rounding)

    structure(
        aov_tab,
        "data" = tidy_to_dataframe(data, formula),
        class = c("oneway.anova_table", "data.frame")
    )
}


# ------------------------------------------------------------------------------------------------ #
#                                        Internal function                                         #
# ------------------------------------------------------------------------------------------------ #
.fisher_anova <- function(
        data,
        formula,
        alpha = 0.05,
        rounding = 4
) {
    lst <- tidy_to_list(data, formula)

    yij <- unlist(lst, use.names = FALSE)  # All observations
    yi <- unlist(lapply(lst, sum), use.names = FALSE)  # Sum of each groups
    k <- length(lst)  # Number of groups
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

    effect_size <- omega_square_partial(DF_between, DF_within, MS_between, MS_within)

    asterisk <- pval2asterisk(pval, break_points = c(alpha + 0.005, alpha, 0.01, 0.001, 0))

    aov_tab <- data.frame(
        row.names   = c("Group", "Residuals", "Total"),
        "DF"        = c(DF_between, DF_within, DF_total),
        "SS"        = round(c(SS_between, SS_within, SS_total), rounding),
        "MS"        = round(c(MS_between, MS_within, NA_real_), rounding),
        "Fvalue"    = round(c(Fval, NA_real_, NA_real_), rounding),
        "Fcrit"     = round(c(Fval_crit, NA_real_, NA_real_), rounding),
        "Pvalue"    = round(c(pval, NA_real_, NA_real_), rounding),
        "signif"    = c(asterisk, NA_character_, NA_character_),
        "p_omega2"  = round(c(effect_size, NA_real_, NA_real_), rounding),
        "method"    = "Fisher's ANOVA"
    )

    return(aov_tab)
}


.welch_anova <- function(
        data,
        formula,
        alpha = 0.05,
        rounding = 4
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

    effect_size <- omega_square_partial(DF_between, DF_within, MS_between, MS_within)

    asterisk <- pval2asterisk(pval, break_points = c(alpha + 0.005, alpha, 0.01, 0.001, 0))

    aov_tab <- data.frame(
        row.names     = c("Group", "Residuals", "Total"),
        "DF"          = round(c(DF_between, DF_within, DF_total), rounding),
        "SS"          = round(c(SS_between, SS_within, SS_total), rounding),
        "MS"          = round(c(MS_between, MS_within, NA_real_), rounding),
        "Fvalue"      = round(c(Fval, NA_real_, NA_real_), rounding),
        "Fcrit"       = round(c(Fval_crit, NA_real_, NA_real_), rounding),
        "Pvalue"      = round(c(pval, NA_real_, NA_real_), rounding),
        "signif"      = c(asterisk, NA_character_, NA_character_),
        "p_omega2"    = round(c(effect_size, NA_real_, NA_real_), rounding),
        "method"      = "Welch's ANOVA"
    )

    return(aov_tab)
}


#' Kruskal–Wallis one-way analysis of variance
#'
#'
#' @param data A data frame containing the response and grouping variables.
#' @param formula A two-sided formula specifying the response and grouping
#'        variables in the form `response ~ group`.
#' @param alpha A numeric significance level used to compute the critical
#'        Chi-squared value. The default is `0.05`.
#' @param rounding Integer (default: 4). Rounding digits.
#' @param silent Logical (default: FALSE). Suppress warnings and messages.
#'
#' @return
#' A data frame representing the ANOVA table with the following columns:
#' \describe{
#'   \item{DF}{Degrees of freedom.}
#'   \item{SS}{Sum of squares.}
#'   \item{MS}{Mean square.}
#'   \item{H}{Observed H statistic.}
#'   \item{Hcrit}{Critical Chi-squared value at the specified significance level.}
#'   \item{Pvalue}{P-value associated with the F statistic.}
#'   \item{signif}{Significance code corresponding to the p-value.}
#'   \item{p_omega2}{Effect size. Partial omega squared.}
#'   \item{method}{Kruskal-Wallis test.}
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
#' Howell, D. C. (2013). Statistical methods for psychology (8th edition).
#' Cengage. Chapter 18, Section 18.9, pg. 678-679.
#'
#' Hollander, M., Wolfe, D. A., & Chicken, E. (2014).
#' Nonparametric Statistical Methods (3rd ed.).
#' Wiley. Chapter 6, pg. 204-206.
#'
#' @examples
#' lst <- list(
#'     "depressant" = c(55, 0, 1, 0, 50, 60, 44),
#'     "stimulant" = c(73, 85, 51, 63, 85, 85, 66, 69),
#'     "placebo" = c(61, 54, 80, 47)
#' )
#'
#' Kruskal_Wallis_test(lst, y ~ x)
#' @export
Kruskal_Wallis_test <- function(
        data,
        formula,
        alpha = 0.05,
        rounding = 4,
        silent = FALSE
) {
    df0 <- tidy_to_dataframe(data, formula)
    df0[["ranked_y"]] <- rank(df0[["y"]])

    # aov_tab <- .fisher_anova(df0, ranked_y ~ x, alpha, rounding)
    # colnames(aov_tab)[colnames(aov_tab) == "Fvalue"] <- "H"
    # colnames(aov_tab)[colnames(aov_tab) == "Fcrit"] <- "Hcrit"

    xi <- df0[["x"]]
    yij <- df0[["ranked_y"]]
    n <- tapply(yij, xi, length)
    N <- sum(n)
    k <- length(unique(xi))
    yi_bar <- tapply(yij, xi, mean)  # group means
    y_bar <- (N + 1) / 2  # grand mean

    DF_between <- k - 1
    DF_within <- N - k
    DF_total <- N - 1

    # Not sure whether the SS_between is valid or not.
    # The formula 6.5 in Hollander et al. (2014) seems very similar
    # to the formula of SS_between in Fisher's ANOVA
    SS_between <- sum(n * ((yi_bar - y_bar) ^ 2))
    SS_within <- NA_real_
    SS_total <- NA_real_

    MS_between <- SS_between / DF_between
    MS_within <- NA_real_

    sum_i <- tapply(df0[["ranked_y"]], df0[["x"]], function(x) sum(x) ^ 2 / length(x))
    ranked_sum <- sum(sum_i)

    ties <- table(df0[["ranked_y"]])
    ties <- 1 - sum(ties ^ 3 - ties) / (N ^ 3 - N)

    H <- (12 / (N * (N + 1)) * ranked_sum - (3 * (N + 1))) / ties
    Hcrit <- stats::qchisq(alpha, DF_between, lower.tail = FALSE)
    pval <- stats::pchisq(H, DF_between, lower.tail = FALSE)

    effect_size <- NA_real_

    asterisk <- pval2asterisk(pval, break_points = c(alpha + 0.005, alpha, 0.01, 0.001, 0))

    aov_tab <- data.frame(
        row.names    = c("Group", "Residuals", "Total"),
        "DF"         = round(c(DF_between, DF_within, DF_total), rounding),
        "SS"         = round(c(SS_between, SS_within, SS_total), rounding),
        "MS"         = round(c(MS_between, MS_within, NA_real_), rounding),
        "H"          = round(c(H, NA_real_, NA_real_), rounding),
        "Hcrit"      = round(c(Hcrit, NA_real_, NA_real_), rounding),
        "Pvalue"     = round(c(pval, NA_real_, NA_real_), rounding),
        "signif"     = c(asterisk, NA_character_, NA_character_),
        "p_omega2"   = round(c(effect_size, NA_real_, NA_real_), rounding),
        "method"     = "Kruskal-Wallis"
    )

    structure(
        aov_tab,
        "data" = df0,
        class = c("oneway.anova_table", "data.frame")
    )
}

