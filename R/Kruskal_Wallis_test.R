#' Kruskal–Wallis one-way analysis of variance by ranks
#'
#' Performs the Kruskal–Wallis rank-sum test for comparing the distributions of two or more
#' independent groups. The test is a nonparametric alternative to one-way ANOVA and is based
#' on the ranks of the observations rather than their original values.
#'
#' @param data A data frame containing the response and grouping variables.
#' @param formula A two-sided formula specifying the response and grouping variables.
#' @param alpha Numeric (default: 0.05). Significance level (range from 0 to 1).
#' @param rounding Integer (default: 4). Number of decimal places displayed in the output.
#' @param silent Logical (default: FALSE). Suppress warnings and messages.
#'
#' @return
#' A data frame summarizing the Kruskal–Wallis test result in an ANOVA-like table:
#' \describe{
#'   \item{DF}{Degrees of freedom.}
#'   \item{SS}{Rank-based between-group sum of squares. Residual and total sums of squares are not
#'             defined for the classical Kruskal–Wallis test and are therefore returned as NA.}
#'   \item{MS}{Rank-based mean square (SS / DF). Only the between-group value is defined.}
#'   \item{H}{Observed Kruskal–Wallis H statistic.}
#'   \item{Hcrit}{Critical chi-squared value at the specified significance level.}
#'   \item{Pvalue}{P-value associated with the H statistic.}
#'   \item{signif}{Significance code corresponding to the p-value.}
#'   \item{p_omega2}{Effect size (currently returned as NA).}
#'   \item{method}{Statistical method ("Kruskal-Wallis").}
#' }
#'
#' The rows correspond to the between-group effect ("Group"), residuals ("Residuals"), and total
#' ("Total"). Only the between-group row contributes to the Kruskal–Wallis test statistic.
#'
#' @details
#' The Kruskal–Wallis test ranks all observations across groups and tests whether the mean ranks
#' differ among groups. Under the null hypothesis, the test statistic approximately follows a
#' chi-squared distribution with k - 1 degrees of freedom, where k is the number of groups.
#' A correction for tied ranks is applied when ties are present.
#'
#' Unlike classical one-way ANOVA, the Kruskal–Wallis test is not derived from a decomposition of
#' variance into between-group and within-group sums of squares. Consequently, residual sums of
#' squares and residual mean squares are not defined in the classical procedure.
#'
#' @references
#' Kruskal, W. H., & Wallis, W. A. (1952).
#' Use of ranks in one-criterion variance analysis.
#' Journal of the American Statistical Association,
#' 47(260), 583--621.
#'
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
#'
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

    MS_between <- NA_real_
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

    # Assign a class of "oneway.art", because the subsequent post-hoc function need to
    # retrieve `ranked_y` from the `data` attribute.
    # The Kruskal-Wallis `ranked_y` calculation is different from the ART-ANOVA.
    structure(
        aov_tab,
        "data" = df0,
        class = c("oneway.anova_table", "oneway.art", "data.frame")
    )
}
