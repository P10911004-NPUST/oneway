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
    ties <- sum(ties ^ 3 - ties)

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
        SE <- sqrt( (N * (N + 1) / 12) - (ties / (12 * (N - 1))) * sum(1 / n) )
        Zval <- abs(diff / SE)
        Zcrit <- stats::qnorm(alpha / 2, lower.tail = FALSE)
        pval <- stats::pnorm(Zval, lower.tail = FALSE)
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

    cld <- compact_letter_display(x1 = post_hoc[["x1"]],
                                  x2 = post_hoc[["x2"]],
                                  pvalues = post_hoc[[padj_name]],
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


if (FALSE)
{
    pond1 <- c(7.68, 7.69, 7.70, 7.70, 7.72, 7.73, 7.73, 7.76)
    pond2 <- c(7.71, 7.73, 7.74, 7.74, 7.78, 7.78, 7.80, 7.81)
    pond3 <- c(7.74, 7.75, 7.77, 7.78, 7.80, 7.81, 7.84)
    pond4 <- c(7.71, 7.71, 7.74, 7.79, 7.81, 7.85, 7.87, 7.91)
}
