t_test <- function(
        data,
        formula,
        alternative = c("two.sided", "less", "greater"),
        alpha = 0.05,
        p_adjust_method = "bonferroni",
        mu = 0,
        var.equal = NA,
        factor_levels = NULL,
        remove_outliers = FALSE,
        ...
) {
    alt <- match.arg(alternative[1], c("two.sided", "less", "greater"))
    ALPHA <- alpha
    alpha <- if (alt == "two.sided") ALPHA / 2 else ALPHA

    if (is.na(var.equal) || !is.logical(var.equal))
        is.var.equal <- varequal::is_var_equal(data, formula, ...)
    else
        is.var.equal <- var.equal

    lst <- tidy_to_list(data, formula, factor_levels)
    grp_names <- names(lst)
    nsub <- length(lst)
    isub <- seq_along(lst)

    summary <- describe(lst, ...)

    #--------------------------------------------------------------------------#
    # One-sample t-test
    #--------------------------------------------------------------------------#
    if (nsub == 1)
    {
        p_adjust_method <- "none"
        pre_hoc <- NULL
        return("Do one-sample test")
    }

    #--------------------------------------------------------------------------#
    # Two-sample t-test
    #--------------------------------------------------------------------------#
    if (nsub == 2)
    {
        y1 <- lst[[1]]
        y2 <- lst[[2]]

        p_adjust_method <- "none"
        pre_hoc <- NULL

        if (isTRUE(is.var.equal))
        {
            post_hoc <- .two_sample_student_t_test(y1, y2, alternative, alpha, mu)
            method <- post_hoc[["method"]]
        } else {
            post_hoc <- .two_sample_welch_t_test(y1, y2, alternative, alpha, mu)
            method <- post_hoc[["method"]]
        }

        post_hoc[["y1"]] <- grp_names[1]
        post_hoc[["y2"]] <- grp_names[2]
    }

    #--------------------------------------------------------------------------#
    # Pairwise t-test
    #--------------------------------------------------------------------------#
    if (nsub > 2)
    {
        method <- sprintf("Pairwise t-test with %s adjustment", p_adjust_method)

        pre_hoc <- NULL

        comb_mat <- utils::combn(names(lst), 2)
        df_lst <- vector("list", ncol(comb_mat))
        for (i in 1:ncol(comb_mat))
        {
            y1_name <- comb_mat[1, i]
            y2_name <- comb_mat[2, i]

            y1 <- lst[[y1_name]]
            y2 <- lst[[y2_name]]

            is.var.equal <- varequal::is_var_equal(list(y1, y2))

            if (isTRUE(is.var.equal))
            {
                post_hoc <- .two_sample_student_t_test(y1, y2, alternative, alpha, mu)
            } else {
                post_hoc <- .two_sample_welch_t_test(y1, y2, alternative, alpha, mu)
            }

            post_hoc[["y1"]] <- y1_name
            post_hoc[["y2"]] <- y2_name
        }
    }

    ret <- oneway_standard_output(
        method = method,
        pre_hoc = pre_hoc,
        post_hoc = post_hoc,
        summary = summary,
        alternative = alternative,
        alpha = ALPHA,
        p_adjust_method = p_adjust_method
    )

    return(ret)

    #----------------------------- Testing ------------------------------------#
    load_all()
    y1 <- c(6625, 6000, 5450, 5200, 5175, 4900, 4750, 4500, 3985,  900,  450, 2800)
    y2 <- c(3900, 3500, 3450, 3200, 2980, 2800, 2500, 2400, 2200, 1200, 1150, 1130)
    df0 <- data.frame(y = c(y1, y2),
                      x = c(rep("Nerve", length(y1)),
                            rep("Muscle", length(y2))))
    out1 <- t_test(df0, y ~ x, var.equal = TRUE)
    print(out1$post_hoc)
    out2 <- t.test(y1, y2, var.equal = TRUE)
    print(out2)
    print(list("tval" = out2$statistic, "pval" = out2$p.value))
}



.two_sample_student_t_test <- function(
        y1,
        y2,
        alternative = c("two.sided", "less", "greater"),
        alpha = 0.05,
        mu = 0
) {
    alt <- match.arg(alternative[1], c("two.sided", "less", "greater"))
    ALPHA <- alpha
    alpha <- if (alt == "two.sided") ALPHA / 2 else ALPHA

    data <- list(y1, y2)

    n <- unlist(lapply(data, length), use.names = FALSE)
    avg <- unlist(lapply(data, mean), use.names = FALSE)
    vars <- unlist(lapply(data, stats::var), use.names = FALSE)

    DF <- sum(n - 1)
    Sp2 <- sum((n - 1) * vars) / DF
    StdErr <- sqrt(sum(Sp2 / n))
    diff <- avg[1] - avg[2]

    tval <- (diff - mu) / StdErr

    tcrit <- stats::qt(alpha, DF, lower.tail = FALSE)
    tcrit <- sign(tval) * tcrit

    pval <- stats::pt(abs(tval), DF, lower.tail = FALSE)
    pval <- if (alt == "two.sided") pval * 2 else pval

    Cohen_d <- diff / sqrt(Sp2)
    Hedges_g <- Cohen_d * (1 - (3 / (4 * sum(n) - 9)))
    effect_size <- stats::setNames(Hedges_g, "Hedges's g")

    t_ncp <- calc_t_ncp(tval, DF, alt, ALPHA)
    ES_CI <- t_ncp * sqrt(sum(1 / n))

    diff_CI_lower <- diff - tcrit * StdErr
    diff_CI_upper <- diff + tcrit * StdErr

    ret <- list(
        "method" = "Student's t",
        "DF" = DF, "Sp2" = Sp2, "SE" = StdErr,
        "tval" = tval, "tcrit" = tcrit, "pval" = pval,
        "diff" = diff, "diff_CI" = c(diff_CI_lower, diff_CI_upper),
        "ES" = effect_size, "ES_CI" = ES_CI
    )

    return(ret)

    #----------------------------- Testing ------------------------------------#
    load_all()
    y1 <- c(16.85, 16.40, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59, 16.57)
    y2 <- c(16.62, 16.75, 17.37, 17.12, 16.98, 16.87, 17.34, 17.02, 17.08, 17.27)
    df0 <- data.frame(y = c(y1, y2),
                      x = c(rep("Modified", length(y1)),
                            rep("Unmodified", length(y2))))
    effectsize::hedges_g(y1, y2)
    .two_sample_student_t_test(y1, y2)
    stats::t.test(y1, y2, var.equal = TRUE)
}


.two_sample_welch_t_test <- function(
        y1,
        y2,
        alternative = c("two.sided", "less", "greater"),
        alpha = 0.05,
        mu = 0
) {
    alt <- match.arg(alternative[1], c("two.sided", "less", "greater"))
    ALPHA <- alpha
    alpha <- if (alt == "two.sided") ALPHA / 2 else ALPHA

    data <- list(y1, y2)

    n <- unlist(lapply(data, length), use.names = FALSE)
    avg <- unlist(lapply(data, mean), use.names = FALSE)
    vars <- unlist(lapply(data, stats::var), use.names = FALSE)

    print(list("n" = n, "mean" = avg, "vars" = vars))

    DF <- sum(vars / n) ^ 2 / sum((vars / n) ^ 2 / (n - 1))
    Sp2 <- sum((n - 1) * vars) / DF  # not used to calculate tval
    StdErr <- sqrt(sum(vars / n))
    diff <- avg[1] - avg[2]

    tval <- (diff - mu) / StdErr

    tcrit <- stats::qt(alpha, DF, lower.tail = FALSE)
    tcrit <- sign(tval) * tcrit

    pval <- stats::pt(abs(tval), DF, lower.tail = FALSE)
    pval <- if (alt == "two.sided") pval * 2 else pval

    Cohen_d <- tval * sqrt(sum(1 / n))
    Hedges_g <- Cohen_d * (1 - (3 / (4 * sum(n) - 9)))
    effect_size <- stats::setNames(Hedges_g, "Hedges's g")

    t_ncp <- calc_t_ncp(tval, sum(n - 1), alt, ALPHA)
    ES_CI <- t_ncp * sqrt(sum(1 / n))

    diff_CI_lower <- diff - tcrit * StdErr
    diff_CI_upper <- diff + tcrit * StdErr

    ret <- list(
        "method" = "Welch's t",
        "DF" = DF, "Sp2" = NA_real_, "SE" = StdErr,
        "tval" = tval, "tcrit" = tcrit, "pval" = pval,
        "diff" = diff, "diff_CI" = c(diff_CI_lower, diff_CI_upper),
        "ES" = effect_size, "ES_CI" = ES_CI
    )

    return(ret)

    #----------------------------- Testing ------------------------------------#
    load_all()
    y1 <- c(6625, 6000, 5450, 5200, 5175, 4900, 4750, 4500, 3985,  900,  450, 2800)
    y2 <- c(3900, 3500, 3450, 3200, 2980, 2800, 2500, 2400, 2200, 1200, 1150, 1130)
    df0 <- data.frame(y = c(y1, y2),
                      x = c(rep("Nerve", length(y1)),
                            rep("Muscle", length(y2))))
    effectsize::hedges_g(y1, y2)
    .two_sample_welch_t_test(y1, y2)
}


