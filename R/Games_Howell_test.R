#' Games-Howell test
#'
#' Represent significance statements resulting from all-pairwise comparisons.
#'
#' @param data A data frame in which the variables specified in the formula will be found.
#' @param formula A formula specifying the model.
#' @param alpha Numeric value range from 0 to 1 (default: 0.05). The error tolerance.
#' @param factor_levels Character vectors (default: NULL). Specify the levels of the factor.
#'        By default, the levels are ordered alphabetically.
#' @param rounding Integer (default: 4). Rounding digits.
#' @param silent Logical (default: FALSE). Raise warning message.
#'
#' @return A list with 4 elements:
#' \describe{
#'   \item{method}{Statistical procedures that were conducted.}
#'   \item{data}{The input data and possibly other transformed data.}
#'   \item{pre_hoc}{*a priori* test result.}
#'   \item{post_hoc}{Post-hoc test result.}
#'   \item{summary}{Descriptive statistics.}
#' }
#'
#' @examples
#' out <- Games_Howell_test(morphine, tolerance ~ grp)
#' out$summary
#' out$post_hoc
#'
#' @references
#' Howell, D. C. (2013). Statistical methods for psychology (8th ed.).
#' Cengage. Chapter 12, pg. 395.
#'
#' @export
Games_Howell_test <- function(
        data,
        formula = NULL,
        alpha = 0.05,
        factor_levels = NULL,
        rounding = 4,
        silent = FALSE
) {
    is_aov <- inherits(data, "oneway.anova_table")
    is_art <- inherits(data, "oneway.art")

    if (isTRUE(is_aov)) {
        pre_hoc <- data
        df0 <- attr(data, "data")
        raw_y <- df0[["y"]]

        if (isTRUE(is_art))
            df0[["y"]] <- df0[["ranked_y"]]
    } else {
        df0 <- tidy_to_dataframe(data, formula, factor_levels)  # from ./tidy_data.R
        pre_hoc <- oneway_anova(df0, y ~ x, alpha, rounding = rounding)  # from ./anova.R
    }

    # -------------------------------------------------------------------------------------- #
    # Check data
    # -------------------------------------------------------------------------------------- #
    if (isFALSE(silent))
    {
        is_normal <- normality::is_normal(df0, y ~ x)
        if (isFALSE(is_normal)) warning("Normality assumption is violated.")
    }

    # -------------------------------------------------------------------------------------- #
    # Summary
    ## The information from the `desc` data frame are (column-wise):
    ## GROUP, CLD, N, AVG, SD, MED, MIN, MAX, CI, SKEW, KURT, normality, n_outliers
    # -------------------------------------------------------------------------------------- #
    desc <- describe(df0, y ~ x, rounding)  # from ./utils.R
    group_names <- desc[["GROUP"]]
    group_sizes <- stats::setNames(desc[["N"]], group_names)
    group_means <- stats::setNames(desc[["AVG"]], group_names)
    group_vars <- stats::setNames(desc[["SD"]] ^ 2, group_names)
    n_grps <- length(group_names)

    DF_within <- pre_hoc[["DF"]][2]  # DFerror: Residuals' degree of freedom
    MS_within <- pre_hoc[["MS"]][2]  # MSE: Mean Square Error

    # -------------------------------------------------------------------------------------- #
    # Group combinations
    # -------------------------------------------------------------------------------------- #
    g_comb <- utils::combn(desc[["GROUP"]], 2)

    post_hoc <- vector("list", ncol(g_comb))
    for (i in 1:ncol(g_comb))
    {
        x1 <- g_comb[1, i]
        x2 <- g_comb[2, i]

        n <- group_sizes[c(x1, x2)]
        vars <- group_vars[c(x1, x2)]

        pooled_var <- sum((n - 1) * vars) / DF_within
        diff <- group_means[[x1]] - group_means[[x2]]
        SE <- sqrt(sum(vars / n) / 2)
        DF_prime <- (sum(vars / n) ^ 2) / sum((vars / n) ^ 2 / (n - 1))
        qval <- abs(diff / SE)
        qcrit <- stats::qtukey(alpha, n_grps, DF_prime, lower.tail = FALSE)  # Studentized range
        pval <- stats::ptukey(qval, n_grps, DF_prime, lower.tail = FALSE)
        diff_CI_lower <- diff - qcrit * SE
        diff_CI_upper <- diff + qcrit * SE

        effect_size <- Hedges_g_s(diff = diff,   # from ./effect_size.R
                                  sample_sizes = n,
                                  pooled_var = pooled_var)

        post_hoc[[i]] <- oneway_post_hoc(  # from ./zzz_standard_output.R
            method = "Games-Howell",
            alternative = "two.sided",
            alpha = alpha,
            mu = 0,
            x1 = x1,
            x2 = x2,
            diff = diff,
            diff_CI = c(diff_CI_lower, diff_CI_upper),
            standard_value = c("qval" = qval),
            critical_value = c("qcrit" = qcrit),
            StdErr = SE,
            Pvalue = pval,
            Padj = pval,
            p_adjust_method = "tukey",
            effect_size = effect_size,
            DF_prime = round(DF_prime, rounding),
            rounding = rounding
        )
    }

    post_hoc <- do.call(rbind.data.frame, post_hoc)

    desc[["CLD"]] <- compact_letter_display(x1 = post_hoc[["x1"]],
                                            x2 = post_hoc[["x2"]],
                                            pvalues = post_hoc[["Pvalue"]],
                                            grp_names = desc[["GROUP"]],
                                            centers = desc[["MED"]],
                                            alpha = alpha)

    if (isTRUE(is_art))
        df0[["y"]] <- raw_y

    oneway_standard_output(  # from ./zzz_standard_output.R
        method = "Games-Howell pairwise comparison",
        data = df0,
        pre_hoc = pre_hoc,
        post_hoc = post_hoc,
        summary = desc
    )
}
