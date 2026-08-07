#' Dunn's multiple comparison test
#'
#' Performs Dunn's multiple comparison procedure for all pairwise comparisons of groups following
#' a significant Kruskal-Wallis test.
#'
#' @param data A data frame, or an object returned by `Kruskal_Wallis_test()`, from which the
#'        response and grouping variables are obtained.
#' @param formula A two-sided formula specifying the response and grouping variables in the form
#'        `response ~ group`. Ignored if `data` is a `"oneway_aov"` object.
#' @param alpha A numeric value between 0 and 1 specifying the significance level.
#'        The default is `0.05`.
#' @param p_adjust_method A character string specifying the method used to adjust p-values for
#'        multiple comparisons. Must be one of `stats::p.adjust.methods`. The default is `"holm"`.
#' @param rounding An integer specifying the number of decimal places to display in the output.
#'        The default is `4`.
#' @param silent Logical. If `FALSE` (default), informative messages or warnings are displayed.
#'
#' @returns
#' A list containing the following components:
#' \describe{
#'   \item{method}{The name of the statistical procedure.}
#'   \item{data}{The input data used in the analysis.}
#'   \item{pre_hoc}{The Kruskal-Wallis test results.}
#'   \item{post_hoc}{Results of all pairwise Dunn comparisons, including differences in mean ranks,
#'                   confidence intervals, Z statistics, unadjusted and adjusted p-values, and
#'                   effect sizes.}
#'   \item{summary}{Descriptive statistics for each group, including compact letter displays (CLD).}
#' }
#'
#' @details
#' Dunn's test is a nonparametric post hoc procedure that compares group mean ranks after a
#' significant Kruskal-Wallis test. The test statistic follows a normal approximation with an
#' adjustment for tied ranks when ties are present.
#'
#' Because multiple pairwise comparisons are performed, p-values should be adjusted to control
#' the family-wise error rate or false discovery rate. The adjustment method is specified by
#' `p_adjust_method` and is passed to `stats::p.adjust()`.
#'
#' If the data satisfy the assumptions of one-way ANOVA, parametric procedures such as
#' `Tukey_HSD_test()`, `REGWQ_test()`, or `Games_Howell_test()` may be more appropriate.
#'
#' @examples
#' out <- Dunn_test(X_X_O, val ~ grp)
#'
#' @references
#' Dinno, A. (2015).
#' Nonparametric Pairwise Multiple Comparisons in Independent Groups using Dunn’s Test.
#' The Stata Journal: Promoting Communications on Statistics and Stata, 15(1), 292-300.
#' https://doi.org/10.1177/1536867X1501500117
#'
#' Zar, J. H. (2014).
#' Biostatistical analysis (5th edition).
#' Pearson. Chapter 11: Multiple comparisons, pg. 255-256.
#'
#' @export
Dunn_test <- function(
        data,
        formula = NULL,
        alpha = 0.05,
        p_adjust_method = "holm",
        rounding = 4,
        silent = FALSE
) {
    if (inherits(data, "oneway_aov"))
        pre_hoc <- data
    else
        pre_hoc <- Kruskal_Wallis_test(data = data,  # from ./anova.R
                                       formula = formula,
                                       alpha = alpha,
                                       rounding = rounding,
                                       silent = TRUE)

    df0 <- attr(pre_hoc, "data")
    df1 <- df0

    if (inherits(data, "oneway_ranked_y") || ! inherits(data, "oneway_art"))
        df1[["y"]] <- df1[["ranked_y"]]
    else
        df1[["y"]] <- rank(df1[["y"]])

    x_name <- attr(df1, "x_name")
    y_name <- attr(df1, "y_name")

    # -------------------------------------------------------------------------------------- #
    # Check data
    # -------------------------------------------------------------------------------------- #
    if (isFALSE(silent))
    {
        is_normal <- normality::is_normal(df1, y ~ x)
        if (is_normal)
            message("Data is normally distributed. Please consider parametric tests.")
    }

    # -------------------------------------------------------------------------------------- #
    # Group summary
    # -------------------------------------------------------------------------------------- #
    xij <- df1[["x"]]
    yij <- df1[["y"]]
    N <- length(yij)
    group_names <- unique(xij)
    group_sizes <- tapply(yij, xij, length)
    group_means <- tapply(yij, xij, mean)
    group_vars <- tapply(yij, xij, stats::var)
    group_medians <- tapply(yij, xij, stats::median)
    n_grps <- length(group_names)

    ties <- table(yij)
    ties_sum <- sum(ties ^ 3 - ties)

    # -------------------------------------------------------------------------------------- #
    # Group combinations
    # -------------------------------------------------------------------------------------- #
    g_comb <- utils::combn(group_names, 2)

    post_hoc <- vector("list", ncol(g_comb))
    for (i in 1:ncol(g_comb))
    {
        x1 <- g_comb[1, i]
        x2 <- g_comb[2, i]

        n <- group_sizes[c(x1, x2)]

        diff <- group_means[[x1]] - group_means[[x2]]
        block_A <- N * (N + 1) / 12
        block_B <- ties_sum / (12 * (N - 1))
        block_C <- sum(1 / n)
        SE <- sqrt((block_A - block_B) * block_C)
        Zval <- abs(diff / SE)
        pval <- stats::pnorm(Zval, lower.tail = FALSE) * 2
        Zcrit <- stats::qnorm(alpha / 2, lower.tail = FALSE)
        diff_CI_lower <- diff - Zcrit * SE
        diff_CI_upper <- diff + Zcrit * SE

        y1 <- yij[xij == x1]
        y2 <- yij[xij == x2]
        effect_size <- Hedges_g_s(y1, y2)

        post_hoc[[i]] <- oneway_post_hoc(  # from ./zzz_standard_output.R
            method = "Dunn",
            alternative = "two.sided",
            alpha = alpha,
            mu = 0,
            x1 = x1,
            x2 = x2,
            diff = diff,
            diff_CI = c(diff_CI_lower, diff_CI_upper),
            standard_value = c("Z" = Zval),
            critical_value = c("Zcrit" = Zcrit),
            StdErr = SE,
            Pvalue = pval,
            Padj = pval,
            p_adjust_method = p_adjust_method,
            effect_size = effect_size,
            rounding = rounding
        )
    }

    post_hoc <- do.call(rbind.data.frame, post_hoc)
    padj_name <- sprintf("Padj (%s)", p_adjust_method)
    post_hoc[[padj_name]] <- stats::p.adjust(post_hoc[["Pvalue"]], method = p_adjust_method)
    post_hoc[["signif"]] <- pval2asterisk(post_hoc[[padj_name]])  # from ./compact_letter_display.R

    cld <- compact_letter_display(x1 = post_hoc[["x1"]],  # from ./compact_letter_display.R
                                  x2 = post_hoc[["x2"]],
                                  pvalues = post_hoc[[padj_name]],
                                  grp_names = group_names,
                                  centers = group_medians,
                                  alpha = alpha)

    desc <- describe(df0, y ~ x, rounding)  # from ./utils.R
    cld <- cld[match(names(cld), desc[["GROUP"]])]
    desc[["CLD"]] <- cld

    ret <- oneway_standard_output(  # from ./zzz_standard_output.R
        method = "Dunn's multiple comparison procedure",
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
        print(post_hoc[, 1:8])
        cat("\n")
        print(desc[, 1:6])
        cat("\n")
    }

    invisible(ret)
}
