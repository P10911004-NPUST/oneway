Tukey_HSD_test <- function(
        data,
        formula = NULL,
        alpha = 0.05,
        factor_levels = NULL,
        remove_outliers = FALSE,
        ...
) {
    if (inherits(data, "oneway.anova_table")) {
        df0 <- attr(data, "data")
        pre_hoc <- data
        DF_within <- pre_hoc[["DF"]][2]  # DFerror: Residuals' degree of freedom
        MS_within <- pre_hoc[["MS"]][2]  # MSE: Mean Square Error
    } else {
        df0 <- tidy_to_dataframe(data, formula, factor_levels)
        pre_hoc <- stats::aov(y ~ x, df0)
        DF_within <- stats::df.residual(pre_hoc)
        SS_within <- sum(stats::residuals(pre_hoc) ^ 2)
        MS_within <- SS_within / DF_within
    }

    #-----------------------------------------------------------------
    # Check data
    #-----------------------------------------------------------------
    is_normal <- normality::is_normal(df0, y ~ x)
    is_var_equal <- varequal::is_var_equal(df0, y ~ x)
    is_balance <- is_balance(df0, y ~ x, buffer_ratio = 0)
    if (isFALSE(is_normal)) warning("Data is not normally distributed.")
    if (isFALSE(is_var_equal)) warning("Homogeneity of variance is not met.")
    if (isFALSE(is_balance)) warning("Sample sizes are not equal.")

    #-----------------------------------------------------------------
    # Summary
    ## The information from the `desc` data frame are (column-wise):
    ## GROUP, CLD, N, AVG, SD, MED, MIN, MAX, CI, SKEW, KURT, normality, n_outliers
    #-----------------------------------------------------------------
    desc <- describe(df0, y ~ x, ...)
    group_names <- desc[["GROUP"]]
    group_sizes <- stats::setNames(desc[["N"]], group_names)
    group_means <- stats::setNames(desc[["AVG"]], group_names)
    group_vars <- stats::setNames(desc[["SD"]] ^ 2, group_names)
    n_grps <- length(group_names)

    #-----------------------------------------------------------------
    # Group combinations
    #-----------------------------------------------------------------
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
        SE <- sqrt(MS_within / mean(group_sizes))
        qval <- abs(diff / SE)
        qcrit <- stats::qtukey(alpha, n_grps, DF_within, lower.tail = FALSE)  # Studentized range
        pval <- stats::ptukey(qval, n_grps, DF_within, lower.tail = FALSE)
        diff_CI_lower <- diff - qcrit * SE
        diff_CI_upper <- diff + qcrit * SE

        effect_size = Hedges_g_s(diff = diff, sample_sizes = n, pooled_var = pooled_var)

        post_hoc[[i]] <- oneway_post_hoc(
            method = "Tukey-HSD",
            alternative = "two.sided",
            alpha = alpha,
            mu = 0,
            x1 = x1,
            x2 = x2,
            diff = diff,
            diff_CI = c(diff_CI_lower, diff_CI_upper),
            standard_value = c("qval" = qval),
            critical_value = c("qcrit" = qcrit),
            Pvalue = pval,
            p_adjust_method = "none",
            effect_size = effect_size
        )
    }

    post_hoc <- do.call(rbind.data.frame, post_hoc)

    desc[["CLD"]] <- compact_letter_display(x1 = post_hoc[["x1"]],
                                            x2 = post_hoc[["x2"]],
                                            pvalues = post_hoc[["Pvalue"]],
                                            grp_names = desc[["GROUP"]],
                                            centers = desc[["AVG"]],
                                            alpha = alpha)

    oneway_standard_output(
        method = "Tukey Honestly Significant Difference (Tukey-HSD) test",
        pre_hoc = NULL,
        post_hoc = post_hoc,
        summary = desc
    )
}


if (FALSE)
{
    load_all()
    df0 <- anorexia
    df0$therapy <- as.factor(df0$therapy)

    aov_mod <- stats::aov(weight_gain ~ therapy, df0)
    res <- agricolae::HSD.test(aov_mod, "therapy")
    res$groups

    out <- Tukey_HSD_test(df0, weight_gain ~ therapy)
    out$summary
}



