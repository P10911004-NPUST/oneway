test_that("Kruskal_Wallis_test", {

    out_1 <- Kruskal_Wallis_test(anorexia, weight_gain ~ therapy, rounding = 7)
    ChiSq_1 <- round(out_1[["H"]][1], 5)
    pval_1 <- round(out_1[["Pvalue"]][1], 7)

    out_2 <- kruskal.test(weight_gain ~ therapy, anorexia)
    ChiSq_2 <- round(unname(out_2[["statistic"]]), 5)
    pval_2 <- round(out_2[["p.value"]], 7)

    testthat::expect_equal(ChiSq_1, ChiSq_2)
    testthat::expect_equal(pval_1, pval_2)
})
