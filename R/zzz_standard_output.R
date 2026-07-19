oneway_standard_output <- function(
        method          = "which priori + post-hoc tests ?",
        pre_hoc         = data.frame(),
        post_hoc        = data.frame(),
        summary         = data.frame(),
        alternative     = c("two.sided", "less", "greater"),
        alpha           = 0.05,
        p_adjust_method = stats::p.adjust.methods,
        checker_func    = list("normality" = normality::is_normal,
                               "variance" = varequal::is_var_equal,
                               "balance" = is_balance,
                               "outliers" = outlying::Grubbs_test),
        pre_hoc_func    = list("O_O" = "Fisher_ANOVA",
                               "O_X" = "Welch_ANOVA",
                               "X_O" = "ART_ANOVA",
                               "X_X" = "ART_ANOVA"),
        post_hoc_func   = list("O_O_O" = "REGWQ_test",
                               "O_O_X" = "Tukey_Kramer_test",
                               "O_X_X" = "Games_Howell_test"),
        plot_param      = list(),
        ...
) {
    list(
        "method"          = method,
        "pre_hoc"         = pre_hoc,
        "post_hoc"        = post_hoc,
        "summary"         = summary,
        "alternative"     = alternative,
        "alpha"           = alpha,
        "p_adjust_method" = p_adjust_method,
        "checker_func"    = checker_func,
        "pre_hoc_func"    = pre_hoc_func,
        "post_hoc_func"   = post_hoc_func,
        "plot_param"      = plot_param,
        ...
    )
}


oneway_post_hoc <- function(
        method = "Which post-hoc test?",
        alternative = "two.sided",
        alpha = 0.05,
        mu = 0,
        y1 = NA_character_,
        y2 = NA_character_,
        diff = NA_real_,
        diff_CI = c(NA_real_, NA_real_),
        standard_value = c("tval" = NA_real_),
        critical_value = c("tcrit" = NA_real_),
        Pvalue = NA_real_,
        p_adjust_method = "none",
        Padj = NA_real_,
        effect_size = NA_real_,
        ...)
{
    df0 <- data.frame(
        row.names = NULL,
        check.names = FALSE,
        "y1"             = y1,
        "y2"             = y2,
        "diff"           = diff,
        "Hedges's g"     = round(effect_size, 4),
        "Pvalue"         = round(Pvalue, 4),
        "Padj"           = round(Padj, 4),
        "diff_CI"        = sprintf("[%.2f, %.2f]", diff_CI[1], diff_CI[2]),
        "mu"             = mu,
        "standard_value" = round(unname(standard_value), 4),
        "critical_value" = round(unname(critical_value), 4),
        "method"         = method,
        "alternative"    = alternative,
        "alpha"          = alpha,
        ...
    )

    conf_lvl <- 100 * (1 - alpha)

    colnames(df0)[colnames(df0) == "diff"] <- "[y1 - y2]"
    colnames(df0)[colnames(df0) == "diff_CI"] <- sprintf("%s%% [y1 - y2] CI", conf_lvl)
    colnames(df0)[colnames(df0) == "standard_value"] <- names(standard_value)
    colnames(df0)[colnames(df0) == "critical_value"] <- names(critical_value)
    colnames(df0)[colnames(df0) == "padj"] <- sprintf("padj (%s)", p_adjust_method)

    return(df0)
}
