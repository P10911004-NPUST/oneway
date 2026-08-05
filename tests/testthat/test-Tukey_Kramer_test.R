test_that("Tukey_Kramer_test", {

    lst <- list(
        data.frame("grp" = "Feb", "val" = c(4.7, 4.9, 5.0, 4.8, 4.7)),
        data.frame("grp" = "May", "val" = c(4.6, 4.4, 4.3, 4.4, 4.1, 4.2)),
        data.frame("grp" = "Aug", "val" = c(4.8, 4.7, 4.6, 4.4, 4.7, 4.8)),
        data.frame("grp" = "Nov", "val" = c(4.9, 5.2, 5.4, 5.1, 5.6))
    )

    df0 <- do.call(rbind.data.frame, lst)

    out_1 <- Tukey_Kramer_test(df0, val ~ grp, rounding = 7, silent = TRUE)
    pval_1 <- round(out_1[["post_hoc"]][["Pvalue"]], 7)

    out_2 <- stats::TukeyHSD(stats::aov(val ~ grp, df0))
    pval_2 <- round(unname(out_2[["grp"]][, "p adj"]), 7)

    testthat::expect_equal(pval_1, pval_2)

})
