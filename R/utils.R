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
                               "balance" = is_balance),
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
        "method" = method,
        "pre_hoc" = pre_hoc,
        "post_hoc" = post_hoc,
        "summary" = summary,
        "alternative" = alternative,
        "alpha" = alpha,
        "p_adjust_method" = p_adjust_method,
        "checker_func" = checker_func,
        "pre_hoc_func" = pre_hoc_func,
        "post_hoc_func" = post_hoc_func,
        "plot_param" = plot_param,
        ...
    )
}


oneway_summary <- function(
        GROUP = NA_character_,
        CLD = NA_character_,
        N = NA_integer_,
        AVG = NA_real_,
        MED = NA_real_,
        SD = NA_real_,
        CI_lower = NA_real_,
        CI_upper = NA_real_,
        MIN = NA_real_,
        MAX = NA_real_,
        skewness = NA_real_,
        kurtosis = NA_real_,
        is_normal = NA,
        n_outliers = NA_integer_,
        ...
) {
    data.frame(
        "GROUP" = GROUP,
        "CLD" = CLD,
        "N" = N,
        "AVG" = AVG,
        "SD" = SD,
        "MED" = MED,
        "MIN" = MIN,
        "MAX" = MAX,
        "CI_lower" = CI_lower,
        "CI_upper" = CI_upper,
        ...
    )
}


oneway_comparison <- function(
        method = "Which post-hoc test?",
        y1 = NA_character_,
        y2 = NA_character_,
        diff = NA_real_,
        standard_value = c("t_val" = NA_real_),
        critical_value = c("t_crit" = NA_real_),
        pvalue = NA_real_,
        padj = c("none" = NA_real_),
        effect_size = c("Cohen's d" = NA_real_),
        CI_lower = NA_real_,
        CI_upper = NA_real_,
        ...)
{
    df0 <- data.frame(
        check.names = FALSE,
        "method" = method,
        "y1" = y1,
        "y2" = y2,
        "y1 - y2" = diff,
        "standard_value" = unname(standard_value),
        "critical_value" = unname(critical_value),
        "pvalue" = pvalue,
        "padj" = unname(padj),
        "effect_size" = unname(effect_size),
        "CI_lower" = CI_lower,
        "CI_upper" = CI_upper,
        ...
    )

    colnames(df0)[colnames(df0) == "padj"] <- sprintf("padj (%s)", names(padj))
    colnames(df0)[colnames(df0) == "standard_value"] <- names(standard_value)
    colnames(df0)[colnames(df0) == "critical_value"] <- names(critical_value)
    colnames(df0)[colnames(df0) == "effect_size"] <- names(effect_size)

    structure(
        .Data = df0,
        class = c("oneway.comparison", "data.frame")
    )
}


tidy_to_list <- function(data, formula, factor_levels = NULL)
{
    # If data is a vector
    if (is.atomic(data) & is.null(dim(data)))
    {
        data <- data[stats::complete.cases(data)]
        lst <- list(data)
        names(lst) <- names(data)
    }

    # If data is a matrix
    if (is.matrix(data))
        data <- as.data.frame(data)

    # If data is a data frame
    if (is.data.frame(data))
    {
        if (is.null(formula) || missing(formula))
            stop("`formula` must be specified, for example: y ~ x")

        all_vars <- all.vars(formula)
        if (length(all_vars) > 2)
        {
            warning(sprintf("Only the first independent variable (%s) is used.", all_vars[2]))
            formula <- sprintf("%s ~ %s", all_vars[1], all_vars[2])
            formula <- stats::as.formula(formula)
        }

        df0 <- stats::model.frame(formula, data, drop.unused.levels = TRUE)
        data <- split(df0[, 1], df0[, 2])
    }

    # If data is a list
    # `is.null(dim(data))` is necessary as data frame is also a kind of list
    if (is.list(data) & is.null(dim(data)))
    {
        data <- lapply(data, function(x) x[stats::complete.cases(x)])
        n <- unlist(lapply(data, length), use.names = FALSE)

        if (any(n < 3))
            warning("All group sizes should be greater than 2.")

        if (is.null(names(data)))
            names(data) <- seq_along(data)

        lst <- data
    }

    if (!is.null(factor_levels) & !missing(factor_levels))
    {
        if (all(names(lst) %in% factor_levels))
            lst <- lst[factor_levels]
        else
            warning("`factor_levels` doesn't match the input data factor levels.")
    }

    return(lst)
}




is_balance <- function(data, formula, buffer_ratio = 0.2)
{
    lst <- tidy_to_list(data, formula)
    n <- unlist(lapply(lst, length), use.names = FALSE)
    if (length(unique(n)) == 1)
        return(TRUE)
    a <- (max(n) / stats::median(n)) <= (1 + buffer_ratio)
    b <- (min(n) / stats::median(n)) >= (1 - buffer_ratio)
    return(a & b)
}


pval2asterisk <- function(
        x,
        break_points = c(0.055, 0.05, 0.01, 0.001, 0),
        symbols = c("ns", ".", "\U273D")
) {
    bp <- break_points[stats::complete.cases(break_points)]
    bp <- sort(bp, decreasing = TRUE)
    n <- length(bp)

    vapply(
        x,
        function(pval)
        {
            if (pval > bp[1])
                return(symbols[1])
            if (pval < bp[1] & pval > bp[2])
                return(symbols[2])
            if (pval <= bp[2] & pval > bp[3])
                return(symbols[3])

            for (i in 3:(n - 1))
            {
                if (pval <= bp[i] & pval > bp[i + 1])
                    ret <- paste(rep(symbols[3], i - 1), collapse = "")
                else
                    next
            }
            return(ret)
        },
        FUN.VALUE = character(1)
    )
}


var_pooled <- function(y, x)
{
    N <- c(tapply(y, x, length))
    VAR <- c(tapply(y, x, stats::var))
    N_ratio <- (N - 1) / sum(N - 1)
    var_pooled <- sum(N_ratio * VAR)
    return(var_pooled)
}


describe <- function(data, formula)
{
    lst <- tidy_to_list(data, formula)
    n_grps <- length(lst)
    mat0 <- vapply(
        X = c("length", "mean", stats::sd, stats::median, "min", "max"),
        function(fn) unlist(lapply(lst, fn)),
        FUN.VALUE = numeric(n_grps)
    )
    mat0 <- mat0[order(mat0[, 4], decreasing = TRUE), ]
    df0 <- as.data.frame(mat0)
    colnames(df0) <- c("N", "AVG", "SD", "MED", "MIN", "MAX")
    return(df0)
}
