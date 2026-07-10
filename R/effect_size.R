Cohen_d <- function(x1, x2)
{
    # Apply to balance and homoscedastic data
    x1_bar <- mean(x1)
    x2_bar <- mean(x2)
    var1 <- stats::var(x1)
    var2 <- stats::var(x2)
    d <- (x1_bar - x2_bar) / sqrt(mean(c(var1, var2)))
    return(d)
}
