test_that("REGWQ_test", {
    pval <- c(0.0858, 0, 0, 0, 0.0001, 0.0001, 0, 0.7258, 0.0470, 0.0411)

    out <- REGWQ_test(morphine, tolerance ~ grp)
    post_hoc <- out$post_hoc

    testthat::expect_equal(round(post_hoc[["Pvalue"]], 4), round(pval, 4))

    # mut <- mutoss::regwq(tolerance ~ grp, morphine, alpha = 0.05)
    # mut <- data.frame(
    #     comparisons = rownames(mut$confIntervals),
    #     confIntervals = mut$confIntervals[, 1, drop = TRUE],
    #     qval = mut$statistic,
    #     padj = mut$adjPValues,
    #     rejected = mut$rejected
    # )
})
