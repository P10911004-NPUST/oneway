#' Scheffé's Method for Planned Contrasts
#'
#' Performs Scheffé's multiple-comparison procedure for testing planned contrasts among group means
#' following an analysis of variance (ANOVA). By default, all pairwise comparisons are performed
#' (NOT recommended, please use other tests to conduct pairwise comparison). Users can also specify
#' custom contrasts through the `planned_contrasts` argument.
#'
#' @param data A data frame, a list, or an object returned by `oneway_anova()`, from which the
#'        response and grouping variables are obtained.
#' @param formula A two-sided formula specifying the response and grouping variables in the form
#'        `response ~ group`. Ignored if `data` is a list or an `"oneway_aov"` object.
#' @param alpha A numeric value between 0 and 1 (default: 0.05) specifying the significance level.
#' @param rounding An integer (default: 4). The number of decimal places to display in the output.
#' @param verbose Logical (default: TRUE). Show warnings and messages.
#' @param planned_contrasts An optional contrast matrix. For example, see `pairwise_contrasts()`.
#'        Each row represents a contrast, and each column corresponds to a group. If `NULL`, all
#'        pairwise contrasts are generated automatically. Custom contrasts should have coefficients
#'        that define the intended linear comparisons among group means.
#'
#' @return An object containing the results of the Scheffé planned-contrast analysis, including
#'         the underlying ANOVA results, group summaries, post-hoc comparison results, and
#'         contrast matrix. The object is returned invisibly.
#'
#' @examples
#' # Compare all pairwise group means using Scheffé's method
#' out <- Scheffe_test(morphine, tolerance ~ grp)
#'
#' # Specify custom contrasts
#' grp_names <- c("MS", "MM", "SS", "SM", "McM")
#' ca <- c(-1, -1, -1, 1.5, 1.5)
#' cb <- c(0, -1, 0, 0, 1)
#' cc <- c(-1, 0, 1, 0, 0)
#' cd <- c(0, 1, -1, 0, 0)
#' contr <- matrix(c(ca, cb, cc, cd), ncol = length(grp_names), byrow = TRUE)
#' colnames(contr) <- grp_names
#' out <- Scheffe_test(morphine, tolerance ~ grp, planned_contrasts = contr)
#'
#' @references
#' Howell, D.C. (2010).
#' Statistical Methods for Psychology (7th ed.).
#' Cengage Learning. Chapter 12: Multiple comparisons among treatment means,
#' Section 12.3: A Priori Comparisons, pg. 374.
#'
#' Heckert, N.A., Filliben, J.J., Croarkin, C.M., Hembree, B., Guthrie, W.F.,
#' Tobias, P., & Prinz, J. (2002).
#' Handbook 151: Nist/sematech e-handbook of statistical methods.
#' Section 7.4.6.2: Scheffe's method
#' https://doi.org/10.18434/M32189
#'
#' @seealso
#' [oneway_anova()], [pairwise_contrasts()]
#'
#' @export
Scheffe_test <- function(
        data,
        formula = NULL,
        alpha = 0.05,
        rounding = 4,
        verbose = TRUE,
        planned_contrasts = NULL
) {
    if (inherits(data, "oneway_aov"))
        pre_hoc <- data
    else
        pre_hoc <- oneway_anova(data = data,  # from ./anova.R
                                formula = formula,
                                alpha = alpha,
                                var_equal = NA,
                                rounding = rounding,
                                verbose = FALSE)

    df0 <- attr(pre_hoc, "data")
    df1 <- df0

    if (inherits(data, "oneway_ranked_y") || inherits(data, "oneway_art"))
        df1[["y"]] <- df1[["ranked_y"]]

    x_name <- attr(df1, "x_name")
    y_name <- attr(df1, "y_name")

    # -------------------------------------------------------------------------------------- #
    # Check data
    # -------------------------------------------------------------------------------------- #
    if (isTRUE(verbose))
    {
        is_normal <- attr(pre_hoc, "is_normal")
        is_var_equal <- attr(pre_hoc, "is_var_equal")
        is_balance <- is_balance(df1, y ~ x)  # from ./utils.R

        if (isFALSE(is_normal))
            warning("Normality assumption is violated.")
        if (isTRUE(is_normal) & isFALSE(is_var_equal))
            message(paste("Homogeneity of variance assumption is violated.",
                          "Please consider Games-Howell test."))
        if (isTRUE(is_normal) & isTRUE(is_var_equal) & isFALSE(is_balance))
            message(paste("Data is unbalance-designed.",
                          "Please consider Tukey-Kramer test."))
    }

    # -------------------------------------------------------------------------------------- #
    # Group summary
    # -------------------------------------------------------------------------------------- #
    xij <- df1[["x"]]
    yij <- df1[["y"]]
    N <- length(yij)
    group_names <- unique(xij)
    group_sizes <- tapply(yij, xij, length)
    group_means <- tapply(yij, xij, mean)
    group_vars <- tapply(yij, xij, stats::var)
    group_medians <- tapply(yij, xij, stats::median)
    n_grps <- length(group_names)

    DF_between <- attr(pre_hoc, "DF_between")
    DF_within <- attr(pre_hoc, "DF_within")  # DFerror: Residuals' degree of freedom
    MS_within <- attr(pre_hoc, "MS_within")  # MSE: Mean Square Error

    # -------------------------------------------------------------------------------------- #
    # Group combinations
    # -------------------------------------------------------------------------------------- #
    if (is.null(planned_contrasts))
        planned_contrasts <- pairwise_contrasts(group_names)

    g_comb <- norm_contr_mat(planned_contrasts)

    if (is.null(colnames(g_comb)))
        colnames(g_comb) <- group_names

    g_comb <- g_comb[, match(group_names, colnames(g_comb)), drop = FALSE]

    post_hoc <- vector("list", ncol(g_comb))
    for (i in 1:nrow(g_comb))
    {
        cc <- g_comb[i, ]  # A named vector of the contrasts
        x1 <- paste(names(cc[cc > 0]), collapse = " + ")
        x2 <- paste(names(cc[cc < 0]), collapse = " + ")

        diff <- sum(cc * group_means)
        SE <- sqrt(MS_within * sum((cc ^ 2) / group_sizes))
        Fval <- abs(diff / SE) ^ 2
        Fcrit <- sqrt(DF_between * stats::qf(alpha, DF_between, DF_within, lower.tail = FALSE))
        pval <- stats::pf(Fval, DF_between, DF_within, lower.tail = FALSE)
        diff_CI_lower <- diff - Fcrit * SE
        diff_CI_upper <- diff + Fcrit * SE

        cohen_d <- diff / SE
        adjust_factor <- 1 - (3 / (4 * N - 9))
        hedges_g <- cohen_d * adjust_factor

        post_hoc[[i]] <- oneway_post_hoc(  # from ./standard_output.R
            method = "Scheffe",
            alternative = "two.sided",
            alpha = alpha,
            mu = 0,
            x1 = x1,
            x2 = x2,
            diff = diff,
            diff_CI = c(diff_CI_lower, diff_CI_upper),
            standard_value = c("F" = Fval),
            critical_value = c("Fcrit" = Fcrit),
            StdErr = SE,
            Pvalue = pval,
            Padj = pval,
            p_adjust_method = "none",
            effect_size = hedges_g,
            rounding = rounding
        )
    }

    post_hoc <- do.call(rbind.data.frame, post_hoc)

    desc <- describe(df0, y ~ x, rounding)  # from ./utils.R

    ret <- oneway_standard_output(  # from ./standard_output.R
        method = "Scheffe's planned contrasts method",
        data = df0,
        pre_hoc = pre_hoc,
        post_hoc = post_hoc,
        summary = desc,
        contrasts = g_comb
    )

    if (isTRUE(verbose))
    {
        DNAME <- deparse(substitute(data))
        dashes <- paste(rep("-", nchar(ret[["method"]]) + 1), collapse = "")
        cat(sprintf("\n%s\n", dashes))
        cat(ret[["method"]])
        cat(sprintf("\n%s\n", dashes))
        cat(sprintf("Data: %s ; Formula: %s ~ %s\n\n", DNAME, y_name, x_name))
        print(post_hoc[, c(1, 2, 3, 5, 7, 8, 10, 11, 12)])
        cat("\n")
    }

    invisible(ret)
}


#' Generate All Pairwise Contrasts
#'
#' Generate a contrast matrix containing all pairwise comparisons among groups. Each row represents
#' one pairwise contrast, with a coefficient of `1` assigned to the first group and `-1` assigned
#' to the second group. All other coefficients are `0`. The sum of all coefficients is zero.
#'
#' @param group_names A character vector of group names or group labels.
#'
#' @return A numeric matrix with one row for each pairwise comparison and one column for each
#'         unique group. Column names correspond to the group names. Each row contains one
#'         coefficient of `1`, one coefficient of `-1`, and zeros elsewhere.
#'
#' @details
#' The contrasts are generated in the order returned by [utils::combn()], with the first group in
#' each pair assigned a coefficient of `1` and the second group assigned a coefficient of `-1`.
#'
#' The resulting matrix can be used to specify all-pairwise comparisons in multiple-comparison
#' procedures, such as Scheffe's test.
#'
#' @examples
#' groups <- c("g1", "g2", "g3", "g4", "g5")
#' pairwise_contrasts(groups)
#'
#' # Use the unique group names from a grouping vector
#' group_names <- c("Control", "Low", "High", "Control", "Low")
#' pairwise_contrasts(group_names)
#'
#' @seealso [utils::combn()]
#'
#' @export
pairwise_contrasts <- function(group_names)
{
    grp_names <- as.character(unique(group_names))
    n_grp <- length(grp_names)

    comb_mat <- utils::combn(1:n_grp, 2)
    n_comb <- ncol(comb_mat)

    contr_mat <- matrix(
        data = vector("integer", n_comb * n_grp),
        ncol = n_grp,
        dimnames = list(NULL, grp_names)
    )

    for (i in 1:n_comb)
    {
        idx_1 <- comb_mat[1, i]
        idx_2 <- comb_mat[2, i]
        contr_mat[i, idx_1] <- 1
        contr_mat[i, idx_2] <- -1
    }

    return(contr_mat)
}


norm_contr_mat <- function(mat)
{
    if ( ! all(apply(mat, 1, sum) == 0) )
        stop("The sum of linear contrasts must be zero. For example, see `pairwise_contrasts()`.")

    n_comb <- nrow(mat)
    n_grp <- ncol(mat)
    mat0 <- matrix(
        vector("integer", n_grp * n_comb),
        ncol = n_grp,
        dimnames = list(rownames(mat), colnames(mat))
    )

    for (i in 1:n_comb)
    {
        vct <- mat[i, , drop = TRUE]
        if (all(vct == 0)) next
        if (sum(vct[vct > 0]) < 1) next
        vct[vct > 0] <- vct[vct > 0] / sum(vct[vct > 0])
        vct[vct < 0] <- vct[vct < 0] / abs(sum(vct[vct < 0]))

        mat0[i, ] <- vct

    }

    return(mat0)
}
