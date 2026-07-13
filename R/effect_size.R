Cohen_d_s <- function(y1, y2, alternative = "two.sided", alpha = 0.05)
{
    # Apply to balance and homoscedastic data
    n1 <- length(y1)
    n2 <- length(y2)
    y1_bar <- mean(y1)
    y2_bar <- mean(y2)
    var1 <- stats::var(y1)
    var2 <- stats::var(y2)

    diff <- (y1_bar - y2_bar)
    Sp <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
    d <- diff / Sp

    # t_ncp0 <- .get_ncp_t(d, n1 + n2 - 2, 1 - alpha)
    t_ncp <- calc_t_ncp(d, n1 + n2 - 2, alternative, alpha)
    np <- (1 / n1 + 1 / n2)
    CI_lower <- t_ncp[1] * sqrt(np)
    CI_upper <- t_ncp[2] * sqrt(np)

    # return(list("t_ncp0" = t_ncp0, "t_ncp" = t_ncp))
    return(list(t_ncp, "d" = d, "d_CI_lower" = CI_lower, "d_CI_upper" = CI_upper))

    #--------------------------------- Testing --------------------------------#
    load_all()
    movie1 <- c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9)
    movie2 <- c(9, 6, 7, 8, 7, 9, 8,  8, 8, 7)
    Cohen_d_s(movie1, movie2)
    effectsize::cohens_d(movie1, movie2)
}


Hedges_g_s <- function(y1, y2, alpha = 0.05)
{
    n1 <- length(y1)
    n2 <- length(y2)
    adjust <- 1 - (3 / (4 * (n1 + n2) - 9))
    d <- Cohen_d_s(y1, y2)
    g <- adjust * d
    return(g)

    #--------------------------------- Testing --------------------------------#
    load_all()
    movie1 <- c(9, 7, 8, 9, 8, 9, 9, 10, 9, 9)
    movie2 <- c(9, 6, 7, 8, 7, 9, 8,  8, 8, 7)
    Hedges_g_s(movie1, movie2)
}


calc_t_ncp <- function(tval, DF, alternative = "two.sided", alpha = 0.05)
{
    if (alternative != "two.sided")
        alpha <- 2 * alpha - 1

    if (alternative == "two.sided")
        probs <- c(alpha / 2, 1 - alpha / 2)
    else
        probs <- c(alpha, 1 - alpha)

    ncp <- suppressWarnings(stats::optim(
        par = c(0, 0),
        fn = function(x) {
            quan <- stats::qt(p = probs, df = DF, ncp = x)
            sum(abs(quan - tval))
        },
        control = list(abstol = 1e-09)
    ))

    t_ncp <- unname(sort(ncp$par))
}


