oneway_standard_output <- function(
        method       = "which priori + post-hoc tests ?",
        data         = NULL,
        pre_hoc      = NULL,
        post_hoc     = NULL,
        summary      = NULL,
        ...
) {
    list(
        "method"     = method,
        "data"       = data,
        "pre_hoc"    = pre_hoc,
        "post_hoc"   = post_hoc,
        "summary"    = summary,
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
        StdErr = NA_real_,
        Pvalue = NA_real_,
        p_adjust_method = "none",
        Padj = NA_real_,
        effect_size = NA_real_,
        rounding = 4,
        ...)
{
    diff_CI <- round(diff_CI, rounding)
    pval <- if (is.null(Padj) || is.na(Padj)) Pvalue else Padj
    asterisks <- pval2asterisk(pval, break_points = c(0.055, alpha, 0.01, 0.001, 0))

    df0 <- data.frame(
        row.names = NULL,
        check.names = FALSE,
        "x1"             = x1,
        "x2"             = x2,
        "diff"           = diff,
        "Hedges's g"     = round(effect_size, rounding),
        "Pvalue"         = round(Pvalue, rounding),
        "Padj"           = round(Padj, rounding),
        "signif"         = asterisks,
        "diff_CI"        = sprintf("[%s, %s]", diff_CI[1], diff_CI[2]),
        "mu"             = mu,
        "standard_value" = round(unname(standard_value), rounding),
        "critical_value" = round(unname(critical_value), rounding),
        "StdErr"         = StdErr,
        "method"         = method,
        "alternative"    = alternative,
        "alpha"          = alpha,
        ...
    )

    conf_lvl <- 100 * (1 - alpha)

    colnames(df0)[colnames(df0) == "diff"] <- "[x1 - x2]"
    colnames(df0)[colnames(df0) == "diff_CI"] <- sprintf("[x1 - x2] %s%% CI", conf_lvl)
    colnames(df0)[colnames(df0) == "standard_value"] <- names(standard_value)
    colnames(df0)[colnames(df0) == "critical_value"] <- names(critical_value)
    colnames(df0)[colnames(df0) == "Padj"] <- sprintf("Padj (%s)", p_adjust_method)

    return(df0)
}
