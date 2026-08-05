#' Ryan-Einot-Gabriel-Welsch Studentized Range (REGWQ) test
#'
#' Performs the Ryan-Einot-Gabriel-Welsch Studentized Range (REGWQ) multiple comparison procedure
#' for all pairwise comparisons of group means following a one-way analysis of variance. REGWQ is
#' a stepwise procedure based on the studentized range distribution that generally provides greater
#' statistical power than Tukey's HSD while maintaining strong control of the family-wise error rate
#' under balanced designs.
#'
#' @param data A data frame, or an object returned by `oneway_anova()`, from which the response
#'        and grouping variables are obtained.
#' @param formula A two-sided formula specifying the response and grouping variables in the form
#'        `response ~ group`. Ignored if `data` is a `"oneway_aov"` object.
#' @param alpha A numeric value between 0 and 1 specifying the significance level.
#'        The default is `0.05`.
#' @param rounding An integer specifying the number of decimal places to display in the output.
#'        The default is `4`.
#' @param silent Logical. If `FALSE` (default), assumption checks are performed and informative
#'        messages or warnings are displayed.
#'
#' @returns
#' A list containing the following components:
#' \describe{
#'   \item{method}{The name of the statistical procedure.}
#'   \item{data}{The input data used in the analysis.}
#'   \item{pre_hoc}{The one-way analysis of variance results.}
#'   \item{post_hoc}{Results of all pairwise REGWQ comparisons, including mean differences,
#'                   confidence intervals, studentized range statistics, p-values, modified
#'                   significance levels, and effect sizes.}
#'   \item{summary}{Descriptive statistics for each group, including compact letter displays (CLD).}
#' }
#'
#' @details
#' The REGWQ procedure is a stepwise multiple comparison method based on the studentized range
#' distribution. Group means are first ordered, after which pairwise comparisons are performed
#' using significance levels that depend on the number of ordered means spanned by each comparison.
#' This adaptive strategy generally yields greater statistical power than Tukey's HSD while
#' maintaining control of the family-wise error rate.
#'
#' The procedure assumes that observations are independent, residuals are approximately normally
#' distributed, and population variances are equal. It is intended primarily for balanced one-way
#' designs. When sample sizes are unequal but variances remain homogeneous, consider using
#' `Tukey_Kramer_test()`. When variances are unequal, consider using `Games_Howell_test()`.
#'
#' @examples
#' out <- REGWQ_test(morphine, tolerance ~ grp)
#'
#' @references
#' Howell, D. C. (2010).
#' Statistical Methods for Psychology (7th ed.).
#' Cengage Learning. Chapter 12: Multiple comparisons among treatment means,
#' Section 12.6: Post hoc comparisons, pg. 393-394.
#'
#' @export
REGWQ_test <- function(
        data,
        formula = NULL,
        alpha = 0.05,
        rounding = 4,
        silent = FALSE
) {
    if (inherits(data, "oneway_aov"))
        pre_hoc <- data
    else
        pre_hoc <- oneway_anova(data, formula, alpha, NA, rounding, silent = TRUE)  # from ./anova.R

    df0 <- attr(pre_hoc, "data")
    df1 <- df0

    if (inherits(data, "oneway_ranked_y") || inherits(data, "oneway_art"))
        df1[["y"]] <- df1[["ranked_y"]]

    x_name <- attr(df1, "x_name")
    y_name <- attr(df1, "y_name")

    # -------------------------------------------------------------------------------------- #
    # Check data
    # -------------------------------------------------------------------------------------- #
    if (isFALSE(silent))
    {
        is_normal <- normality::is_normal(df1, y ~ x)
        is_var_equal <- varequal::is_var_equal(df1, y ~ x)
        is_balance <- is_balance(df1, y ~ x)  # from ./utils.R

        if (isFALSE(is_normal))
            warning("Normality assumption is violated.")
        if (isTRUE(is_normal) & isFALSE(is_var_equal))
            message(paste("Homogeneity of variance assumption is violated.",
                          "Please consider Games-Howell test."))
        if (isTRUE(is_normal) & isTRUE(is_var_equal) & isFALSE(is_balance))
            message(paste("Data is unbalance-designed.",
                          "Please consider Tukey-Kramer test."))
    }

    # -------------------------------------------------------------------------------------- #
    # Group summary
    # -------------------------------------------------------------------------------------- #
    xij <- df1[["x"]]
    yij <- df1[["y"]]
    N <- length(yij)
    group_sizes <- tapply(yij, xij, length)
    group_means <- tapply(yij, xij, mean)
    group_vars <- tapply(yij, xij, stats::var)
    group_medians <- tapply(yij, xij, stats::median)
    group_names <- names(sort(group_means, decreasing = TRUE))
    n_grps <- length(group_names)

    DF_within <- attr(pre_hoc, "DF_within")  # DFerror: Residuals' degree of freedom
    MS_within <- attr(pre_hoc, "MS_within")  # MSE: Mean Square Error

    # ----------------------------------------------------------------- #
    # Group combinations
    # ----------------------------------------------------------------- #
    g_comb <- utils::combn(group_names, 2)
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

        y1 <- yij[xij == x1]
        y2 <- yij[xij == x2]
        effect_size <- Hedges_g_s(y1, y2)  # from ./effect_size.R

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

    # The Padj should be compared with the modified alpha, not the original alpha.
    # Labeling with different number of asterisks to show the level of significance seems not feasible.
    # So, just label with 3 asterisks to show significance.
    post_hoc <- do.call(rbind.data.frame, post_hoc)
    signif <- post_hoc[["Pvalue"]] < post_hoc[["modified_alpha"]]
    post_hoc[["signif"]] <- vapply(signif,
                                   function(x) switch(x + 1, "ns", "***"),
                                   character(1))

    cld <- compact_letter_display(x1 = post_hoc[["x1"]],  # from ./compact_letter_display.R
                                  x2 = post_hoc[["x2"]],
                                  pvalues = post_hoc[["Pvalue"]],
                                  grp_names = group_names,
                                  centers = group_medians,
                                  alpha = alpha)

    desc <- describe(df0, y ~ x, rounding)  # from ./utils.R
    cld <- cld[match(names(cld), desc[["GROUP"]])]
    desc[["CLD"]] <- cld

    ret <- oneway_standard_output(  # from ./zzz_standard_output.R
        method = "REGWQ multiple comparison procedure",
        data = df0,
        pre_hoc = pre_hoc,
        post_hoc = post_hoc,
        summary = desc
    )

    if (isFALSE(silent))
    {
        DNAME <- deparse(substitute(data))
        dashes <- paste(rep("-", nchar(ret[["method"]]) + 1), collapse = "")
        cat(sprintf("\n%s\n", dashes))
        cat(ret[["method"]])
        cat(sprintf("\n%s\n", dashes))
        cat(sprintf("Data: %s ; Formula: %s ~ %s\n\n", DNAME, y_name, x_name))
        print(post_hoc[, c(1:6, 16, 7, 8)])
        cat("\n")
        print(desc[, 1:6])
        cat("\n")
    }

    invisible(ret)
}

