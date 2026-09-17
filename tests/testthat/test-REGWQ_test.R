test_that("REGWQ_test", {
    qval <- c(2.1596, 7.5727,  8.7806, 13.8509, 16.5565,
              5.4131, 6.6210, 11.6913, 14.3969,  1.2079,
              6.2783, 8.9838,  5.0704,  7.7759,  2.7056)

    pval <- c(0.1295, 0.0000, 0.0000, 0.0000, 0.0000,
              0.0002, 0.0000, 0.0000, 0.0000, 0.3948,
              0.0001, 0.0000, 0.0005, 0.0000, 0.0582)

    out <- REGWQ_test(O_O_O, val ~ grp, verbose = FALSE)
    post_hoc <- out$post_hoc

    testthat::expect_equal(round(post_hoc[["q"]], 4), round(qval, 4))
    testthat::expect_equal(round(post_hoc[["Pvalue"]], 4), round(pval, 4))

    # mut <- mutoss::regwq(val ~ grp, O_O_O, alpha = 0.05)
    # mut <- data.frame(
    #     comparisons = rownames(mut$confIntervals),
    #     confIntervals = mut$confIntervals[, 1, drop = TRUE],
    #     qval = mut$statistic,
    #     padj = mut$adjPValues,
    #     rejected = mut$rejected
    # )
})
