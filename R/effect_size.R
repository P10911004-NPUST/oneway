Cohen_d_s <- function(
        diff,
        sample_sizes,
        standard_value,
        pooled_var,
        DF_total,
        alternative = "two.sided",
        alpha = 0.05,
        mu = 0,
        dist_func = stats::qt
) {
    d <- (diff - mu) / sqrt(pooled_var)
    ncp <- calc_ncp(standard_value, DF_total, alternative, alpha, dist_func)
    se <- sqrt(sum(1 / sample_sizes))
    ci_lower <- ncp[1] * se
    ci_upper <- ncp[2] * se
    ret <- c("Cohen's d" = d, "CI_lower" = ci_lower, "CI_upper" = ci_upper)
    return(ret)
    #-------------------------------- Testing --------------------------------#
    load_all()
    y1 <- c(16.85, 16.40, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59, 16.57)
    y2 <- c(16.62, 16.75, 17.37, 17.12, 16.98, 16.87, 17.34, 17.02, 17.08, 17.27)
    sample_sizes <- c(length(y1), length(y2))

    t_out <- .two_sample_student_t_test(y1, y2)
    Cohen_d_s(t_out$diff, sample_sizes, t_out$tval, t_out$pooled_var, t_out$DF_total)
    ef_out <- effectsize::cohens_d(y1, y2)
    print(c("Cohen's d" = ef_out$Cohens_d, "CI_lower" = ef_out$CI_low, "CI_upper" = ef_out$CI_high))

    t_out <- .two_sample_welch_t_test(y1, y2)
    Cohen_d_s(t_out$diff, sample_sizes, t_out$tval, t_out$pooled_var, t_out$DF_total)
    ef_out <- effectsize::cohens_d(y1, y2, pooled_sd = FALSE)
    print(c("Cohen's d" = ef_out$Cohens_d, "CI_lower" = ef_out$CI_low, "CI_upper" = ef_out$CI_high))
}


Hedges_g_s <- function(
        diff,
        sample_sizes,
        standard_value,
        pooled_var,
        DF_total,
        alternative = "two.sided",
        alpha = 0.05,
        mu = 0,
        dist_func = stats::qt
) {
    d <- (diff - mu) / sqrt(pooled_var)
    g <- d * (1 - (3 / (4 * sum(sample_sizes) - 9)))
    ncp <- calc_ncp(standard_value, DF_total, alternative, alpha, dist_func)
    se <- sqrt(sum(1 / sample_sizes))
    ci_lower <- ncp[1] * se
    ci_upper <- ncp[2] * se
    ret <- c("Hedges's g" = g, "CI_lower" = ci_lower, "CI_upper" = ci_upper)
    return(ret)
    #-------------------------------- Testing --------------------------------#
    load_all()
    y1 <- c(16.85, 16.40, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59, 16.57)
    y2 <- c(16.62, 16.75, 17.37, 17.12, 16.98, 16.87, 17.34, 17.02, 17.08, 17.27)
    sample_sizes <- c(length(y1), length(y2))

    t_out <- .two_sample_student_t_test(y1, y2, mu = 1)
    Hedges_g_s(t_out$diff, sample_sizes, t_out$tval, t_out$pooled_var, t_out$DF_total, mu = 1)
    ef_out <- effectsize::hedges_g(y1, y2, mu = 1)
    print(c("Hedges's g" = ef_out$Hedges_g, "CI_lower" = ef_out$CI_low, "CI_upper" = ef_out$CI_high))

    t_out <- .two_sample_welch_t_test(y1, y2, mu = 1)
    Hedges_g_s(t_out$diff, sample_sizes, t_out$tval, t_out$pooled_var, t_out$DF_total, mu = 1)
    ef_out <- effectsize::hedges_g(y1, y2, pooled_sd = FALSE, mu = 1)
    print(c("Hedges's g" = ef_out$Hedges_g, "CI_lower" = ef_out$CI_low, "CI_upper" = ef_out$CI_high))
}

# Hedges_g_s <- function(
        #         centers,
#         variances,
#         sample_sizes,
#         mu = 0,
#         alternative = "two.sided",
#         alpha = 0.05,
#         dist_func = stats::qt
# ) {
#     DF <- sum(sample_sizes - 1)
#     diff <- centers[1] - centers[2]
#     pooled_vars <- sum((sample_sizes - 1) * variances) / DF
#     d <- diff / sqrt(pooled_vars)
#     adjust <- 1 - (3 / (4 * sum(sample_sizes) - 9))
#     g <- adjust * d
#
#     StdErr <- sqrt(sum(pooled_vars / sample_sizes))
#     standard_value <- (diff - mu) / StdErr
#
#     ncp <- calc_ncp(standard_value, DF, alternative, alpha, dist_func)
#
#     ci_lower <- ncp[1] * sqrt(sum(1 / n))
#     ci_upper <- ncp[2] * sqrt(sum(1 / n))
#
#     ret <- c("Hedges's g" = g, "CI_lower" = ci_lower, "CI_upper" = ci_upper)
#     return(ret)
#
#     #-------------------------------- Testing --------------------------------#
#     load_all()
#     y1 <- c(16.85, 16.40, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59, 16.57)
#     y2 <- c(16.62, 16.75, 17.37, 17.12, 16.98, 16.87, 17.34, 17.02, 17.08, 17.27)
#     avg <- c(mean(y1), mean(y2))
#     vars <- c(stats::var(y1), stats::var(y2))
#     n <- c(length(y1), length(y2))
#     Hedges_g_s(avg, vars, n)
#     effectsize::hedges_g(y1, y2)
# }



calc_ncp <- function(
        standard_value,
        degree_freedom,
        alternative = "two.sided",
        alpha = 0.05,
        dist_func = stats::qt
) {
    conf_int_points <- c(NA_real_, NA_real_)
    if (alternative == "two.sided")
    {
        alpha <- alpha / 2
        conf_int_points <- c(alpha, 1 - alpha)
    } else {
        if (alternative == "less")
            conf_int_points <- c(alpha, Inf)
        if (alternative == "greater")
            conf_int_points <- c(-Inf, alpha)
    }

    if (abs(standard_value) < 2)
        init_value <- sign(standard_value) * 2
    else
        init_value <- standard_value / 2

    suppressWarnings(
        ncp <- stats::optim(par = c(init_value, init_value),
                            fn = function(x)
                            {
                                qt_points <- dist_func(p = conf_int_points,
                                                       df = degree_freedom,
                                                       ncp = x)
                                err <- sum(abs(qt_points - standard_value))
                                return(err)
                            },
                            control = list(abstol = 1e-09))

    )

    t_ncp <- unname(sort(ncp$par))
    return(t_ncp)
}


