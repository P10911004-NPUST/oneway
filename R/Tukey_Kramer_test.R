#' Tukey-Kramer test
#'
#' Represent significance statements resulting from all-pairwise comparisons.
#'
#' @param data A data frame in which the variables specified in the formula will be found.
#' @param formula A formula specifying the model.
#' @param alpha Numeric value range from 0 to 1 (default: 0.05). The error tolerance.
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
#' out <- Tukey_Kramer_test(O_O_X, val ~ grp)
#'
#' @references
#' Howell, D. C. (2013).
#' Statistical Methods for Psychology (8th ed.).
#' Cengage Learning. Chapter 12: Multiple comparisons among treatment means,
#' Section 12.7: Tukey's test, pg. 394.
#'
#' Zar, J. H. (2014).
#' Biostatistical analysis (5th edition).
#' Pearson. Chapter 11: Multiple comparisons, pg. 244-245.
#'
#' @export
Tukey_Kramer_test <- function(
        data,
        formula = NULL,
        alpha = 0.05,
        rounding = 4,
        silent = FALSE
) {
    if (inherits(data, "oneway_aov"))
        pre_hoc <- data
    else
        pre_hoc <- oneway_anova(data = data,  # from ./anova.R
                                formula = formula,
                                alpha = alpha,
                                var_equal = NA,
                                rounding = rounding,
                                silent = TRUE)

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

        if ( ! is_normal )
            warning("Normality assumption is violated.")
        if ( is_normal & ! is_var_equal )
            message(paste("Homogeneity of variance assumption is violated.",
                          "Please consider Games-Howell test."))
        if ( is_normal & is_var_equal & is_balance )
            message(paste("Data is balance-designed.",
                          "Please consider Tukey-HSD test."))
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

    DF_within <- attr(pre_hoc, "DF_within")  # DFerror: Residuals' degree of freedom
    MS_within <- attr(pre_hoc, "MS_within")  # MSE: Mean Square Error

    # ----------------------------------------------------------------- #
    # Group combinations
    # ----------------------------------------------------------------- #
    g_comb <- utils::combn(group_names, 2)

    post_hoc <- vector("list", ncol(g_comb))
    for (i in 1:ncol(g_comb))
    {
        x1 <- g_comb[1, i]
        x2 <- g_comb[2, i]

        n <- group_sizes[c(x1, x2)]
        vars <- group_vars[c(x1, x2)]

        # pooled_var <- sum((n - 1) * vars) / DF_within
        diff <- group_means[[x1]] - group_means[[x2]]
        SE <- sqrt(sum(MS_within / n) / 2)  # The only difference from Tukey-HSD
        qval <- abs(diff / SE)
        qcrit <- stats::qtukey(alpha, n_grps, DF_within, lower.tail = FALSE)  # Studentized range
        pval <- stats::ptukey(qval, n_grps, DF_within, lower.tail = FALSE)
        diff_CI_lower <- diff - qcrit * SE
        diff_CI_upper <- diff + qcrit * SE

        y1 <- yij[xij == x1]
        y2 <- yij[xij == x2]
        effect_size <- Hedges_g_s(y1, y2)  # from ./effect_size.R

        post_hoc[[i]] <- oneway_post_hoc(  # from ./zzz_standard_output.R
            method = "Tukey-Kramer",
            alternative = "two.sided",
            alpha = alpha,
            mu = 0,
            x1 = x1,
            x2 = x2,
            diff = diff,
            diff_CI = c(diff_CI_lower, diff_CI_upper),
            standard_value = c("q" = qval),
            critical_value = c("qcrit" = qcrit),
            StdErr = SE,
            Pvalue = pval,
            Padj = pval,
            p_adjust_method = "tukey",
            effect_size = effect_size,
            rounding = rounding
        )
    }

    post_hoc <- do.call(rbind.data.frame, post_hoc)

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
        method = "Tukey-Kramer multiple comparison procedure",
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
