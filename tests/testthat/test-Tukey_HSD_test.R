test_that("Tukey_HSD_test", {

    mod_1 <- Tukey_HSD_test(O_O_O, val ~ grp, rounding = 7, silent = TRUE)
    pval <- sort(mod_1[["post_hoc"]][["Pvalue"]])

    mod_2 <- stats::TukeyHSD(stats::aov(val ~ grp, O_O_O))
    padj <- sort(round(unname(mod_2[["grp"]][, "p adj"]), 7))

    testthat::expect_equal(pval, padj)

})
