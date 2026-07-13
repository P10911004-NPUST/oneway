


# Confidence interval for a population mean
CI_population <- function(y, alpha = 0.05, mu, t_crit, se)
{
    if (missing(mu) || missing(t_crit) || missing(se))
    {
        if (missing(y)) stop("Please specify `y`.")
        n <- length(y)
        mu <- mean(y)
        t_crit <- stats::qt(alpha / 2, n - 1, lower.tail = FALSE)
        se <- stats::sd(y) / sqrt(n)
    }

    CI_lower <- mu - t_crit * se  # lower-limit of confidence interval
    CI_upper <- mu + t_crit * se  # upper-limit of confidence interval

    ret <- c("CI_lower" = CI_lower,
             "CI_upper" = CI_upper,
             "CI" = (1 - alpha))

    return(ret)

    #----------------------------------- Testing ------------------------------#
    y <- c(35, 36, 44, 54, 43, 36, 25, 41, 40, 29)
    CI_population(y)
}


CI_coerce_to_character <- function(lower, upper)
{
    vct <- sprintf("[%s, %s]", round(lower, 2), round(upper, 2))
    return(vct)
}
