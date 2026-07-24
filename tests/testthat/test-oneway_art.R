test_that("oneway_art", {

    art_mod <- oneway_art(anorexia, weight_gain ~ therapy, rounding = 7)
    pval <- round(art_mod[["Pvalue"]][1], 7)

    testthat::expect_equal(pval, 0.0088171)

})
