test_that("Tukey_HSD_test", {
    mod_1 <- Tukey_HSD_test(morphine, tolerance ~ grp, rounding = 7)
    pval <- sort(mod_1[["post_hoc"]][["Pvalue"]])

    mod_2 <- stats::TukeyHSD(stats::aov(tolerance ~ grp, morphine))
    padj <- sort(round(unname(mod_2[["grp"]][, "p adj"]), 7))

    testthat::expect_equal(pval, padj)
})
