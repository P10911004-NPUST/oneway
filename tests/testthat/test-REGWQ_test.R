test_that("REGWQ_test", {
    qval <- c(2.0840, 7.3075, 8.4731, 15.9767, 5.2235, 6.3891, 13.8927, 1.1656, 8.6692, 7.5036)
    pval <- c(0.1439, 0.0000, 0.0000, 0.0000, 0.0004, 0.0001, 0.0000, 0.4119, 0.0000, 0.0000)

    out <- REGWQ_test(O_O_O, val ~ grp, silent = TRUE)
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
