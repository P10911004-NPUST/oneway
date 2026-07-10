oneway_standard_output <- function(
        method = "which priori + post-hoc tests ?",
        pre_hoc = data.frame(),
        post_hoc = data.frame(),
        summary = data.frame(),
        alpha = 0.05,
        p_adjust_method = stats::p.adjust.methods,
        is_normal = logical(1),
        is_var_equal = logical(1),
        is_balance = logical(1),
        checker_func = list("outliers" = outlying::Grubbs_test,
                            "normality" = normality::is_normal,
                            "variance" = varequal::is_var_equal,
                            "balance" = is_balance),
        pre_hoc_func = list("O_O" = "Fisher_ANOVA",
                            "O_X" = "Welch_ANOVA",
                            "X_O" = "ART_ANOVA",
                            "X_X" = "ART_ANOVA"),
        post_hoc_func = list("O_O_O" = "REGWQ_test",
                             "O_O_X" = "Tukey_Kramer_test",
                             "O_X_X" = "Games_Howell_test"),
        plot_param = list()
) {
    list(
        method,
        pre_hoc,
        post_hoc,
        summary,
        data,
        formula,
        alpha,
        p_adjust_method,
        is_normal,
        is_var_equal,
        is_balance,
        checker_func,
        pre_hoc_func,
        post_hoc_func
    )
}


tidy_to_dataframe <- function(data, formula = NULL)
{
    ret <- NULL

    # Data frame is also a kind of list, so `is.null(dim(data))` is necessary
    if (is.list(data) & is.null(dim(data)))
    {
        data <- lapply(data, function(x) x[stats::complete.cases(x)])
        isub <- seq_along(data)

        if (is.null(names(data)))
            names(data) <- as.character(isub)

        grp <- names(data)

        lst <- lapply(isub,
                      function(i)
                      {
                          vct <- data[[i]]
                          vct <- vct[stats::complete.cases(vct)]
                          if (is.null(vct) || length(vct) == 0)
                              df0 <- data.frame(y = NA_real_, x = grp[i])
                          else
                              df0 <- data.frame(y = vct, x = grp[i])
                      })
        ret <- do.call(rbind.data.frame, lst)
        ret <- ret[stats::complete.cases(ret[["y"]]), ]
        ret[["x"]] <- as.character(ret[["x"]])
        attr(ret, "x_name") <- "IV"
        attr(ret, "y_name") <- "DV"
    }

    if (is.matrix(data))
        data <- as.data.frame(data)

    if (is.data.frame(data))
    {
        if (is.null(formula) || missing(formula))
            stop("Please specify the `formula`, for example: lhs ~ rhs")
        df0 <- stats::model.frame(formula, data, drop.unused.levels = TRUE)
        x_name <- colnames(df0)[2]
        y_name <- colnames(df0)[1]
        colnames(df0) <- c("y", "x")
        df0[["x"]] <- as.character(df0[["x"]])
        ret <- df0[stats::complete.cases(df0[["y"]]), ]
        attr(ret, "x_name") <- x_name
        attr(ret, "y_name") <- y_name
    }

    ret <- ret[order(ret[["x"]]), ]

    return(ret)
}


is_balance <- function(data, formula, buffer_ratio = 0.2)
{
    df0 <- tidy_to_dataframe(data, formula)
    n <- tapply(df0[["y"]], df0[["x"]], length)
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


VAR_pooled <- function(val, grp)
{
    N <- c(tapply(val, grp, length))
    VAR <- c(tapply(val, grp, stats::var))
    N_ratio <- (N - 1) / sum(N - 1)
    var_pooled <- sum(N_ratio * VAR)
    return(var_pooled)
}


describe <- function(data, formula)
{
    df0 <- tidy_to_dataframe(data, formula)
    n_grps <- length(unique(df0[["x"]]))
    mat0 <- vapply(
        X = c("length", "mean", stats::sd, stats::median, "min", "max"),
        function(fn) tapply(df0[["y"]], df0[["x"]], fn),
        FUN.VALUE = numeric(n_grps)
    )
    mat0 <- mat0[order(mat0[, 4], decreasing = TRUE), ]
    ret <- as.data.frame(mat0)
    colnames(ret) <- c("N", "AVG", "SD", "MED", "MIN", "MAX")
    return(ret)
}
