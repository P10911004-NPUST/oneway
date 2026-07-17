# Confidence interval for a population mean
CI_pop_mean <- function(y, alternative = "two.sided", alpha = 0.05, mu, t_crit, se)
{
    ALPHA <- alpha
    if (alternative == "two.sided")
        alpha <- alpha / 2

    if (missing(mu) || missing(t_crit) || missing(se))
    {
        if (missing(y)) stop("Please specify `y`.")
        n <- length(y)
        mu <- mean(y)
        t_crit <- stats::qt(alpha, n - 1, lower.tail = FALSE)
        se <- stats::sd(y) / sqrt(n)
    }

    CI_lower <- mu - t_crit * se  # lower-limit of confidence interval
    CI_upper <- mu + t_crit * se  # upper-limit of confidence interval

    if (alternative == "less")
        CI_lower <- sign(CI_lower) * Inf
    if (alternative == "greater")
        CI_upper <- sign(CI_upper) * Inf

    ret <- c("CI_lower" = CI_lower,
             "CI_upper" = CI_upper,
             "CI (%)" = 100 * (1 - ALPHA))

    return(ret)
}

