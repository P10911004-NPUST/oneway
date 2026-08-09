#' Multiple comparison procedure
#'
#' Performs a one-way analysis of variance followed by an appropriate post-hoc multiple pairwise
#' comparison based on the distributional characteristics and design of the data. The procedure
#' evaluates normality, homogeneity of variances, and sample-size balance to select an appropriate
#' analysis protocol.
#'
#' If the response variable is normally distributed, the post-hoc procedure is selected
#' according to variance homogeneity and sample-size balance:
#' \itemize{
#'     \item equal variances and balanced group sizes: REGWQ test;
#'     \item equal variances and unbalanced group sizes: Tukey-Kramer test;
#'     \item unequal variances: Games-Howell test.
#' }
#'
#' If the response variable is not normally distributed, an aligned-rank-transform (ART)
#' analysis is first attempted. Normality and variance homogeneity are then reassessed using
#' the aligned-ranked response. If the aligned-ranked response is normally distributed, the
#' same variance- and balance-based selection described above is applied. Otherwise, the procedure
#' uses the Kruskal-Wallis test followed by Dunn's multiple-comparison test.
#'
#' The resulting object contains the selected pre-hoc and post-hoc analysis results and identifies
#' the complete analysis protocol in the method component.
#'
#' @param data A data frame containing the response and grouping variables.
#' @param formula A two-sided formula specifying the response and grouping variables in the form
#'        of `response ~ group`.
#' @param alpha A numeric significance level used for hypothesis testing. The default is 0.05.
#' @param rounding An integer specifying the number of decimal places used when reporting numerical
#'        results. The default is 4.
#' @param silent A logical value indicating whether the results should be printed to the console.
#'        If `FALSE`, the default, the selected analysis protocol, post-hoc comparisons, and
#'        summary are printed. If `TRUE`, no output is printed.
#' @param p_adjust_method A character string specifying the method used to adjust p-values for
#'        Dunn's multiple-comparison test when the Kruskal-Wallis/Dunn protocol is selected. The
#'        default is "holm". See `stats::p.adjust()` for available methods.
#'
#' @return A list containing the results of the selected analysis protocol. The method component
#'         identifies the complete protocol, combining the *a priori* and post-hoc procedures.
#'         The object also contains the post-hoc results and summary produced by the selected
#'         procedure.
#'
#' @details
#' The analysis proceeds according to the following decision sequence:
#'
#' \enumerate{
#'     \item A preliminary one-way ANOVA is performed to assess normality and homoscedasticity.
#'     \item If the response is normally distributed, the post-hoc procedure is selected according
#'           to variance homogeneity and sample-size balance.
#'     \item If the response is not normally distributed, an ART analysis is performed.
#'     \item Normality and variance homogeneity are reassessed using the aligned-ranked response
#'           from the ART analysis.
#'     \item If the aligned-ranked response remains non-normal, the Kruskal-Wallis test followed by
#'           Dunn's test is used.
#'     \item Otherwise, the REGWQ, Tukey-Kramer, or Games-Howell procedure is selected according to
#'           variance homogeneity and sample-size balance.
#' }
#'
#' Thus, the possible analysis protocols are:
#' \itemize{
#'     \item one-way ANOVA + REGWQ;
#'     \item one-way ANOVA + Tukey-Kramer;
#'     \item one-way ANOVA + Games-Howell;
#'     \item ART-ANOVA + REGWQ;
#'     \item ART-ANOVA + Tukey-Kramer;
#'     \item ART-ANOVA + Games-Howell;
#'     \item Kruskal-Wallis + Dunn's test.
#' }
#'
#' The function is intended as a general-purpose adaptive procedure for one-factor experimental
#' designs. The selected method depends on the diagnostic results obtained from the supplied data
#' and should therefore be interpreted together with the underlying assumptions and study design.
#'
#' @examples
#' # Normally distributed, variance equal, balanced groups
#' pairwise_comparison(O_O_O, val ~ grp)
#'
#' # Normally distributed, variance equal, unbalanced groups
#' pairwise_comparison(O_O_X, val ~ grp)
#'
#' # Normally distributed, variance unequal
#' pairwise_comparison(O_X_X, val ~ grp)
#'
#' # Non-parametric
#' pairwise_comparison(X_X_X, val ~ grp)
#'
#' @seealso
#' [oneway_anova()], [oneway_art()], [REGWQ_test()],
#' [Tukey_Kramer_test()], [Games_Howell_test()],
#' [Kruskal_Wallis_test()], [Dunn_test()]
#'
#' @export
pairwise_comparison <- function(
        data,
        formula,
        alpha = 0.05,
        rounding = 4,
        silent = FALSE,
        p_adjust_method = "holm"
) {
    pre_hoc <- oneway_anova(data = data,
                            formula = formula,
                            alpha = alpha,
                            var_equal = NA,
                            rounding = rounding,
                            silent = TRUE)

    df0 <- attr(pre_hoc, "data")

    is_normal <- attr(pre_hoc, "is_normal")
    is_var_equal <- attr(pre_hoc, "is_var_equal")
    is_balance <- is_balance(data, formula)

    # When the data is non-normal, try ART-ANOVA
    if ( ! is_normal )
    {
        pre_hoc <- oneway_art(data = data,
                              formula = formula,
                              alpha = alpha,
                              var_equal = NA,
                              rounding = rounding,
                              silent = TRUE)

        df0 <- attr(pre_hoc, "data")

        is_normal <- normality::is_normal(df0, ranked_y ~ x)
        is_var_equal <- varequal::is_var_equal(df0, ranked_y ~ x)

        # If the aligned-ranked y is still non-normal, then use Kruskal-Wallis + Dunn's test
        if ( ! is_normal )
        {
            ret <- Dunn_test(data = data,
                             formula = formula,
                             alpha = alpha,
                             p_adjust_method = p_adjust_method,
                             rounding = rounding,
                             silent = TRUE)

            pre_hoc_method <- ret[["pre_hoc"]][["method"]][1]
            post_hoc_method <- ret[["method"]]
            protocol <- sprintf("%s + %s", pre_hoc_method, post_hoc_method)
        }
    }

    # Either the `y` or `ranked_y` is normally distributed, conduct the same procedure
    # the only difference is using either raw y or aligned-ranked y as the response variable
    if (is_normal)
    {
        if (is_var_equal & is_balance)
            ret <- REGWQ_test(pre_hoc, alpha = alpha, rounding = rounding, silent = TRUE)

        if (is_var_equal & ! is_balance)
            ret <- Tukey_Kramer_test(pre_hoc, alpha = alpha, rounding = rounding, silent = TRUE)

        if ( ! is_var_equal )
            ret <- Games_Howell_test(pre_hoc, alpha = alpha, rounding = rounding, silent = TRUE)

        pre_hoc_method <- ret[["pre_hoc"]][["method"]][1]
        post_hoc_method <- ret[["method"]]
        protocol <- sprintf("%s + %s", pre_hoc_method, post_hoc_method)
    }

    ret[["method"]] <- protocol

    if (isFALSE(silent))
    {
        DNAME <- deparse(substitute(data))
        y_name <- attr(df0, "y_name")
        x_name <- attr(df0, "x_name")

        dashes <- paste(rep("-", nchar(post_hoc_method) + 1), collapse = "")
        cat(sprintf("\n%s\n", dashes))
        cat(sprintf("%s +\n", pre_hoc_method))
        cat(sprintf("%s", post_hoc_method))
        cat(sprintf("\n%s\n", dashes))
        cat(sprintf("Data: %s ; Formula: %s ~ %s\n\n", DNAME, y_name, x_name))
        print(ret[["post_hoc"]][, 1:8])
        cat("\n")
        print(ret[["summary"]][, 1:6])
        cat("\n")
    }

    invisible(ret)
}
