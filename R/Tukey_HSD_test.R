Tukey_HSD_test <- function(
        data,
        formula,
        alpha,
        factor_levels = NULL,
        remove_outliers = FALSE,
        ...
) {
    lst <- tidy_to_list(data, formula, factor_levels)
    ni <- unlist(lapply(lst, length), use.names = FALSE)

    if (length(unique(ni)) > 1)
        stop("All group sample sizes should be equal.")

    # MSE <- unlist(lapply(lst, mse), use.names = FALSE)
    # AVG <- unlist(lapply(lst, mean))
    #
    # ymax <- max(AVG)
    # ymin <- min(AVG)
    #
    # q <- (ymax - ymin) / sqrt(MSE / mean(ni))
    #
    # DF_within <- sum(ni - 1)
}
