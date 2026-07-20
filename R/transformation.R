transform_options <- c("sq", "sqrt", "log", "log2", "log10", "recip")

transform <- function(y, method = "sq")
{
    method <- match.arg(tolower(method[1]), transform_options)
    m <- match(method, transform_options)

    func <- switch(m,
                   .transform_sq,
                   .transform_sqrt,
                   .transform_log,
                   .transform_log2,
                   .transform_log10,
                   .transform_reciprocal)

    y_prime <- func(y)

    return(y_prime)
}



.transform_sq <- function(y)
{
    # can reduce left skewness (move the peak towards left)
    y * y
}


.transform_sqrt <- function(y)
{
    # can reduce heteroscedasticity
    gmin <- min(y)  # grand minimum
    gmax <- max(y)  # grand maximum
    y_prime <- (y - gmin) / (gmax - gmin)
    sqrt(y_prime)
}


.transform_log <- function(y)
{
    # can reduce right skewness (move the peak towards right)
    gmin <- min(y)  # grand minimum
    gmax <- max(y)  # grand maximum
    y_prime <- (y - gmin) / (gmax - gmin) + 1
    log(y_prime)
}


.transform_log2 <- function(y)
{
    # can reduce right skewness (move the peak towards right)
    gmin <- min(y)  # grand minimum
    gmax <- max(y)  # grand maximum
    y_prime <- (y - gmin) / (gmax - gmin) + 1
    log2(y_prime)
}


.transform_log10 <- function(y)
{
    # can reduce right skewness (move the peak towards right)
    gmin <- min(y)  # grand minimum
    gmax <- max(y)  # grand maximum
    y_prime <- (y - gmin) / (gmax - gmin) + 1
    log10(y_prime)
}


.transform_reciprocal <- function(y)
{
    # can reduce heteroscedasticity and kurtosis, and also move the peak towards center
    gmin <- min(y)  # grand minimum
    gmax <- max(y)  # grand maximum
    y_prime <- (y - gmin) / (gmax - gmin) + 1
    1 / y_prime
}



.transform_art <- function(data, formula, simplify = FALSE, misc = FALSE)
{
    df0 <- tidy_to_dataframe(data, formula)
    y_bar <- mean(df0[["y"]])

    aov_mod <- stats::aov(y ~ x, df0)
    df0[["residuals"]] <- stats::residuals(aov_mod)

    yij <- split(df0[["y"]], df0[["x"]])
    residuals <- split(df0[["residuals"]], df0[["x"]])
    estimated_effect <- tapply(df0[["y"]], df0[["x"]], function(x) mean(x) - y_bar)

    aligned_y <- lapply(names(yij),
                        function(nm)
                        {
                            estimated_effect <- mean(yij[[nm]]) - y_bar
                            aligned_y <- residuals[[nm]] + estimated_effect
                            rounding <- abs(ceiling(log10(.Machine$double.eps) / 2))
                            aligned_y <- round(aligned_y, rounding)
                            return(aligned_y)
                        })
    aligned_y <- unlist(aligned_y, use.names = FALSE)

    ranked_y <- rank(unlist(aligned_y), ties.method = "average")

    if (isTRUE(simplify))
    {
        ret <- ranked_y
    } else {
        df0[["aligned_y"]] <- aligned_y
        df0[["ranked_y"]] <- ranked_y
        ret <- df0
    }

    if (isTRUE(misc))
    {
        ret <- list(
            "data" = df0,
            "estimated_effect" = estimated_effect
        )
    }

    return(ret)
}
