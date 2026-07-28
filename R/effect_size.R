#' Effect size
#'
#' Calculate Cohen's d and Hedges' g introduced in Lakens (2013).
#'
#' @param y1 A numeric vector.
#' @param y2 A numeric vector.
#' @param alternative Character (default: `"two.sided"`). Specifies the alternative hypothesis.
#'        Available options are `c("two.sided", "less", "greater")`.
#' @param alpha Numeric (default: 0.05). Significance level (0 - 1) for hypothesis testing.
#' @param mu Numeric (default: 0).
#' @param return_CI Logical (default: FALSE). Whether to return the confidence interval of the effect size.
#'
#' @returns A numeric scalar or a numeric vector of length 3, depends on `return_CI`.
#'
#' @details
#' Refer to Lakens (2013), the Cohen's d is the formula 1, the Hedges' g is the formula 4.
#' Their confidence interval (CI) were calculated based on formula 2, by transforming the `d` and `g`
#' back to `t` to fit the general confidence interval calculation.
#' For detailed and precise CI estimates, please use the `effectsize` package.
#'
#' @references
#' Lakens, D. (2013).
#' Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs.
#' Frontiers in Psychology, 4.
#' https://doi.org/10.3389/fpsyg.2013.00863
#' @export
Cohen_d_s <- function(y1, y2, alternative = "two.sided", alpha = 0.05, mu = 0, return_CI = FALSE)
{
    alt <- match.arg(alternative[1], c("two.sided", "less", "greater"))
    ALPHA <- alpha
    alpha <- if (alt == "two.sided") ALPHA / 2 else ALPHA

    data <- list(y1, y2)

    is_var_equal <- varequal::is_var_equal(data)

    n <- unlist(lapply(data, length), use.names = FALSE)
    avg <- unlist(lapply(data, mean), use.names = FALSE)
    vars <- unlist(lapply(data, stats::var), use.names = FALSE)

    if (isTRUE(is_var_equal))
    {
        DF_within <- sum(n - 1)
        pooled_var <- sum((n - 1) * vars) / DF_within
        StdErr <- sqrt(sum(pooled_var / n))
    }
    else {
        DF_within <- sum(vars / n) ^ 2 / sum((vars / n) ^ 2 / (n - 1))
        pooled_var <- mean(vars)
        StdErr <- sqrt(sum(vars / n))
    }

    diff <- avg[1] - avg[2]

    cohen_d <- (diff - mu) / sqrt(pooled_var)
    ret <- c("Cohen's d" = cohen_d)

    # ------------------------------------------------------------------------------------------- #
    #                              Confidence interval of effect size                             #
    # ------------------------------------------------------------------------------------------- #
    if (isTRUE(return_CI))
    {
        tval <- (diff - mu) / StdErr
        init_val <- if (abs(tval) < 2) sign(tval) * 2 else tval / 2

        # --------------------------- Non-centrality parameters (NCP) -------------------------- #
        conf_int_points <- c(NA_real_, NA_real_)
        if (alt == "two.sided")
        {
            alpha <- alpha / 2
            conf_int_points <- c(alpha, 1 - alpha)
        } else {
            if (alt == "less")
                conf_int_points <- c(alpha, Inf)
            if (alt == "greater")
                conf_int_points <- c(-Inf, alpha)
        }

        suppressWarnings(
            ncp <- stats::optim(par = c(init_val, init_val),
                                fn = function(x)
                                {
                                    qt_points <- stats::qt(p = conf_int_points,
                                                           df = DF_within,
                                                           ncp = x)
                                    err <- sum(abs(qt_points - tval))
                                    return(err)
                                },
                                control = list(abstol = 1e-09))
        )

        ncp <- unname(sort(ncp$par))

        SE <- sqrt(sum(1 / n))
        ci_lower <- ncp[1] * SE
        ci_upper <- ncp[2] * SE

        ret <- c("Cohen's d" = cohen_d, "CI_lower" = ci_lower, "CI_upper" = ci_upper)
    }

    return(ret)
}


#' @rdname Cohen_d_s
#' @export
Hedges_g_s <- function(y1, y2, alternative = "two.sided", alpha = 0.05, mu = 0, return_CI = FALSE)
{
    out <- Cohen_d_s(y1, y2, alternative, alpha, mu, return_CI)
    cohen_d <- out[[1]]
    N <- length(y1) + length(y2)
    adjust_factor <- 1 - (3 / (4 * N - 9))
    hedges_g <- cohen_d * adjust_factor

    if (isTRUE(return_CI))
    {
        ci_lower <- out[[2]] * adjust_factor
        ci_upper <- out[[3]] * adjust_factor
        ret <- c("Hedges' g" = hedges_g, "CI_lower" = ci_lower, "CI_upper" = ci_upper)
    } else {
        ret <- c("Hedges' g" = hedges_g)
    }

    return(ret)
}


omega_square_partial <- function(
        DF_between,
        DF_within,
        MS_between,
        MS_within
) {
    num <- DF_between * (MS_within - MS_between)
    denom <- DF_between * MS_between + (DF_within + 1) * MS_within
    abs(num / denom)
}
