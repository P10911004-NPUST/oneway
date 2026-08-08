test_that("compact_letter_display", {
    out <- Dunn_test(X_X_O, val ~ grp, silent = TRUE)
    tab <- row_arrange(out[["summary"]], "MED")
    post <- out[["post_hoc"]]
    cld <- compact_letter_display(x1 = post$x1,
                                  x2 = post$x2,
                                  pvalues = post$`Padj (holm)`,
                                  grp_names = tab$GROUP,
                                  centers = tab$MED)
    expect_letters <- stats::setNames(c("d", "cd", "bc", "bc", "b", "a"),
                                      paste0("G", c(3, 6, 2, 5, 4, 1)))

    testthat::expect_equal(cld, expect_letters)
})
