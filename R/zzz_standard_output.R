oneway_standard_output <- function(
        method          = "which priori + post-hoc tests ?",
        pre_hoc         = NULL,
        post_hoc        = NULL,
        summary         = NULL,
        ...
) {
    list(
        "method"          = method,
        "pre_hoc"         = pre_hoc,
        "post_hoc"        = post_hoc,
        "summary"         = summary,
        ...
    )
}


oneway_post_hoc <- function(
        method = "Which post-hoc test?",
        alternative = "two.sided",
        alpha = 0.05,
        mu = 0,
        x1 = NA_character_,
        x2 = NA_character_,
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
        "x1"             = x1,
        "x2"             = x2,
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

    colnames(df0)[colnames(df0) == "diff"] <- "[x1 - x2]"
    colnames(df0)[colnames(df0) == "diff_CI"] <- sprintf("%s%% [x1 - x2] CI", conf_lvl)
    colnames(df0)[colnames(df0) == "standard_value"] <- names(standard_value)
    colnames(df0)[colnames(df0) == "critical_value"] <- names(critical_value)
    colnames(df0)[colnames(df0) == "padj"] <- sprintf("padj (%s)", p_adjust_method)

    return(df0)
}
