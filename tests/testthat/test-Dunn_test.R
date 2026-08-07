test_that("Dunn_test", {

    df0 <- data.frame(
        pond1 = c(7.68, 7.69, 7.70, 7.70, 7.72, 7.73, 7.73, 7.76),
        pond2 = c(7.71, 7.73, 7.74, 7.74, 7.78, 7.78, 7.80, 7.81),
        pond3 = c(7.74, 7.75, 7.77, 7.78, 7.80, 7.81, 7.84, NA),
        pond4 = c(7.71, 7.71, 7.74, 7.79, 7.81, 7.85, 7.87, 7.91)
    )

    df0 <- df_wide_to_long(df0, 1:4)

    out <- Dunn_test(df0, val ~ grp, rounding = 7, silent = TRUE)
    Zval <- round(out[["post_hoc"]][["Z"]], 7)
    pval <- round(out[["post_hoc"]][["Padj (holm)"]], 7)

    expect_Zval <- c(2.1370063, 2.9493489, 2.9918088, 0.8848047, 0.8548025, 0.0589870)
    expect_pval <- c(0.1303900, 0.0166398, 0.0166398, 1.0000000, 1.0000000, 1.0000000)

    testthat::expect_equal(Zval, expect_Zval)
    testthat::expect_equal(pval, expect_pval)

})
