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
        pre_hoc <- oneway_anova(data, formula, alpha, NA, rounding, silent = TRUE)  # from ./anova.R

    df0 <- attr(pre_hoc, "data")
    df1 <- df0

    if (inherits(data, "oneway_art"))
        df1[["y"]] <- df1[["ranked_y"]]

    # -------------------------------------------------------------------------------------- #
    # Check data
    # -------------------------------------------------------------------------------------- #
    if (isFALSE(silent))
    {
        is_normal <- normality::is_normal(df1, y ~ x)
        is_var_equal <- varequal::is_var_equal(df1, y ~ x)
        is_balance <- is_balance(df1, y ~ x)

        if (all(is_normal, is_var_equal, is_balance))
            message("Normality, homoscedasticity, and balance-designed assumption is met.
                    Please consider REGWQ or Tukey-HSD test.")
        if (all(is_normal, is_var_equal, isFALSE(is_balance)))
            message("Normality and homoscedasticity assumption is met.
                    Please consider Tukey-Kramer test.")
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
        vars <- group_vars[c(x1, x2)]

        diff <- group_means[[x1]] - group_means[[x2]]
        SE <- sqrt(sum(vars / n) / 2)
        DF_prime <- (sum(vars / n) ^ 2) / sum((vars / n) ^ 2 / (n - 1))
        qval <- abs(diff / SE)
        qcrit <- stats::qtukey(alpha, n_grps, DF_prime, lower.tail = FALSE)
        pval <- stats::ptukey(qval, n_grps, DF_prime, lower.tail = FALSE)
        diff_CI_lower <- diff - qcrit * SE
        diff_CI_upper <- diff + qcrit * SE

        y1 <- yij[xij == x1]
        y2 <- yij[xij == x2]
        effect_size <- Hedges_g_s(y1, y2)

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

    cld <- compact_letter_display(x1 = post_hoc[["x1"]],
                                  x2 = post_hoc[["x2"]],
                                  pvalues = post_hoc[["Pvalue"]],
                                  grp_names = group_names,
                                  centers = group_medians,
                                  alpha = alpha)

    desc <- describe(df0, y ~ x, rounding)
    cld <- cld[match(names(cld), desc[["GROUP"]])]
    desc[["CLD"]] <- cld

    ret <- oneway_standard_output(  # from ./zzz_standard_output.R
        method = "Dunn's multiple comparison procedure",
        data = df0,
        pre_hoc = pre_hoc,
        post_hoc = post_hoc,
        summary = desc
    )

    invisible(ret)
}
