#' Ryan, Einot, Gabriel, Welsh Studentized Range Q (REGWQ) test
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
#' out <- REGWQ_test(morphine, tolerance ~ grp)
#' out$pre_hoc
#' out$summary
#' out$post_hoc
#'
#' @references
#' Howell, D. C. (2010). Statistical methods for psychology (7th ed.).
#' Cengage. Chapter 12, pg. 393-394.
#'
#' @export
REGWQ_test <- function(
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

        if (isTRUE(is_art))
        {
            colnames(df0)[colnames(df0) == "y"] <- "raw_y"
            df0[["y"]] <- df0[["ranked_y"]]
        }

    } else {
        df0 <- tidy_to_dataframe(data, formula, factor_levels)  # from ./tidy_data.R
        pre_hoc <- oneway_anova(df0, y ~ x, alpha, rounding = rounding)  # from ./anova.R
    }

    # ----------------------------------------------------------------- #
    # Check data
    # ----------------------------------------------------------------- #
    if (isFALSE(silent))
    {
        is_normal <- normality::is_normal(df0, y ~ x)
        is_var_equal <- varequal::is_var_equal(df0, y ~ x)
        is_balance <- is_balance(df0, y ~ x, buffer_ratio = 0)
        if (isFALSE(is_normal)) warning("Normality assumption is violated.")
        if (isFALSE(is_var_equal)) warning("Homogeneity of variance assumption is violated.")
        if (isFALSE(is_balance)) warning("Sample sizes are not equal.")
    }

    # ----------------------------------------------------------------- #
    # Summary
    ## The information from the `desc` data frame are (column-wise):
    ## GROUP, CLD, N, AVG, SD, MED, MIN, MAX, CI, SKEW, KURT, normality, n_outliers
    # ----------------------------------------------------------------- #
    desc <- describe(df0, y ~ x, rounding)  # from ./utils.R
    desc <- desc[order(desc[["AVG"]], decreasing = TRUE), ]

    group_names <- desc[["GROUP"]]
    group_sizes <- stats::setNames(desc[["N"]], group_names)
    group_means <- stats::setNames(desc[["AVG"]], group_names)
    group_vars <- stats::setNames(desc[["SD"]] ^ 2, group_names)
    n_grps <- length(group_names)

    DF_within <- pre_hoc[["DF"]][2]  # DFerror: Residuals' degree of freedom
    MS_within <- pre_hoc[["MS"]][2]  # MSE: Mean Square Error

    # ----------------------------------------------------------------- #
    # Group combinations
    # ----------------------------------------------------------------- #
    g_comb <- utils::combn(desc[["GROUP"]], 2)
    step_size <- utils::combn(1:n_grps, 2)
    step_size <- abs(step_size[1, ] - step_size[2, ]) + 1

    post_hoc <- vector("list", ncol(g_comb))
    for (i in 1:ncol(g_comb))
    {
        x1 <- g_comb[1, i]
        x2 <- g_comb[2, i]

        r <- step_size[i]
        if (r < (n_grps - 1))
            modified_alpha <- 1 - (1 - alpha) ^ (r / n_grps)
        else
            modified_alpha <- alpha

        n <- group_sizes[c(x1, x2)]
        vars <- group_vars[c(x1, x2)]

        pooled_var <- sum((n - 1) * vars) / DF_within
        diff <- group_means[[x1]] - group_means[[x2]]
        SE <- sqrt(MS_within / mean(group_sizes))
        qval <- abs(diff / SE)
        qcrit <- stats::qtukey(modified_alpha, r, DF_within, lower.tail = FALSE)
        pval <- stats::ptukey(qval, r, DF_within, lower.tail = FALSE)
        diff_CI_lower <- diff - qcrit * SE
        diff_CI_upper <- diff + qcrit * SE

        y1 <- df0[df0[["x"]] == x1, ][["y"]]
        y2 <- df0[df0[["x"]] == x2, ][["y"]]
        effect_size <- Hedges_g_s(y1, y2)

        post_hoc[[i]] <- oneway_post_hoc(  # from ./zzz_standard_output.R
            method = "REGWQ",
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
            modified_alpha = round(modified_alpha, rounding),
            rounding = rounding
        )
    }

    post_hoc <- do.call(rbind.data.frame, post_hoc)
    signif <- post_hoc[["Pvalue"]] < post_hoc[["modified_alpha"]]
    post_hoc[["signif"]] <- vapply(signif,
                                   function(x) switch(x + 1, "ns", "***"),
                                   character(1))

    desc[["CLD"]] <- compact_letter_display(x1 = post_hoc[["x1"]],  # from ./compact_letter_display.R
                                            x2 = post_hoc[["x2"]],
                                            pvalues = post_hoc[["Pvalue"]],
                                            grp_names = desc[["GROUP"]],
                                            centers = desc[["MED"]],
                                            alpha = modified_alpha)

    ret <- oneway_standard_output(  # from ./zzz_standard_output.R
        method = "REGWQ pairwise comparison",
        data = df0,
        pre_hoc = pre_hoc,
        post_hoc = post_hoc,
        summary = desc
    )

    invisible(ret)
}



if (FALSE)
{
    load_all()

    # m1 <- c(51, 84, 50, 48, 79, 61, 53, 54)
    # m2 <- c(82, 91, 92, 80, 52, 85, 73, 74)
    # m3 <- c(79, 84, 74, 98, 63, 83, 85, 58)
    # m4 <- c(85, 80, 65, 71, 67, 51, 63, 93)
    # m5 <- c(37, 40, 61, 51, 76, 55, 60, 70)
    #
    # df0 <- data.frame(
    #     grp = rep(c("m1", "m2", "m3", "m4", "m5"), each = 8),
    #     val = c(m1, m2, m3, m4, m5)
    # )
    #
    # out <- REGWQ_test(df0, val ~ grp, rounding = 7)
    # post_hoc <- (out$post_hoc)

    # REGWQ_test(morphine, tolerance ~ grp)
    # Tukey_HSD_test(morphine, tolerance ~ grp)

    mut <- mutoss::regwq(tolerance ~ grp, morphine, alpha = 0.05)
    mut <- data.frame(
        comparisons = rownames(mut$confIntervals),
        confIntervals = mut$confIntervals[, 1, drop = TRUE],
        qval = mut$statistic,
        padj = mut$adjPValues,
        rejected = mut$rejected
    )

    regwq <- REGWQ_test(morphine, tolerance ~ grp)
    regwq <- regwq$post_hoc

    print(mut)
    print(regwq)
}



