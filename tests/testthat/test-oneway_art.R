test_that("oneway_art", {

    art_mod <- oneway_art(anorexia, weight_gain ~ therapy)
    pval <- round(art_mod[["Pvalue"]][1], 5)

    testthat::expect_equal(pval, 0.00882)
})
