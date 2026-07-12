t_test <- function(
        data,
        formula,
        alternative = c("two.sided", "less", "greater"),
        alpha = 0.05,
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
    nsub <- length(lst)
    isub <- seq_along(lst)

    # Perform one-sample test
    if (nsub == 1)
    {
        cat("Do one-sample test")
    }

    if (nsub == 2)
    {
        y1 <- lst[[1]]
        y2 <- lst[[2]]

        if (isTRUE(is.var.equal))
            out <- .two_sample_student_t_test(y1, y2, alternative, alpha, mu)
        else
            out <- .two_sample_welch_t_test(y1, y2, alternative, alpha, mu)
    }

    if (nsub > 2)
    {
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
                out <- .two_sample_student_t_test(y1, y2, alternative, alpha, mu)
            else
                out <- .two_sample_welch_t_test(y1, y2, alternative, alpha, mu)
        }
    }
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
    Diff <- avg[1] - avg[2] - mu

    t_val <- Diff / StdErr

    t_crit <- stats::qt(alpha, DF, lower.tail = FALSE)
    t_crit <- sign(t_val) * t_crit

    pval <- stats::pt(abs(t_val), DF, lower.tail = FALSE)
    pval <- if (alt == "two.sided") pval * 2 else pval

    effect_size <- stats::setNames(NA_real_, "Cohen d")

    ret <- oneway_comparison(
        method = "Student's t",
        y1 = "y1",
        y2 = "y2",
        diff = Diff,
        standard_value = c("t_val" = t_val),
        critical_value = c("t_crit" = t_crit),
        pvalue = pval,
        effect_size = effect_size,
        CI_lower = NA_real_,
        CI_upper = NA_real_
    )

    return(ret)

    #----------------------------- Testing ------------------------------------#
    y1 <- c(16.85, 16.40, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59, 16.57)
    y2 <- c(16.62, 16.75, 17.37, 17.12, 16.98, 16.87, 17.34, 17.02, 17.08, 17.27)
    df0 <- data.frame(y = c(y1, y2),
                      x = c(rep("Modified", length(y1)),
                            rep("Unmodified", length(y2))))
    .two_sample_student_t_test(y1, y2)
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
    StdErr <- sqrt(sum(vars / n))
    Diff <- avg[1] - avg[2] - mu

    t_val <- Diff / StdErr

    t_crit <- stats::qt(alpha, DF, lower.tail = FALSE)
    t_crit <- sign(t_val) * t_crit

    pval <- stats::pt(abs(t_val), DF, lower.tail = FALSE)
    pval <- if (alt == "two.sided") pval * 2 else pval

    effect_size <- stats::setNames(NA_real_, "Cohen d")

    ret <- oneway_comparison(
        method = "Welch's t",
        y1 = "y1",
        y2 = "y2",
        diff = Diff,
        standard_value = c("t_val" = t_val),
        critical_value = c("t_crit" = t_crit),
        pvalue = pval,
        effect_size = effect_size,
        CI_lower = NA_real_,
        CI_upper = NA_real_
    )

    return(ret)

    #----------------------------- Testing ------------------------------------#
    y1 <- c(6625, 6000, 5450, 5200, 5175, 4900, 4750, 4500, 3985,  900,  450, 2800)
    y2 <- c(3900, 3500, 3450, 3200, 2980, 2800, 2500, 2400, 2200, 1200, 1150, 1130)
    df0 <- data.frame(y = c(y1, y2),
                      x = c(rep("Nerve", length(y1)),
                            rep("Muscle", length(y2))))
    .two_sample_welch_t_test(y1, y2)
}


