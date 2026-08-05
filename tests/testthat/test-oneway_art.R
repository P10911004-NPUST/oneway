test_that("oneway_art", {

    art_mod <- oneway_art(X_X_O, val ~ grp, silent = FALSE)
    Fval_1 <- round(attr(art_mod, "Fvalue"), 4)
    pval_1 <- round(attr(art_mod, "Pvalue"), 20)

    # art_ <- ARTool::art(val ~ as.factor(grp), X_X_O)
    # aov_mod <- stats::anova(art_)
    # Fval_2 <- round(aov_mod$`F value`, 4)
    # pval_2 <- round(aov_mod$`Pr(>F)`, 20)

    testthat::expect_equal(Fval_1, 21.2401)
    testthat::expect_equal(pval_1, 5.81037e-15)

    art_mod <- oneway_art(X_X_X, val ~ grp, silent = FALSE)
    Fval_1 <- round(attr(art_mod, "Fvalue"), 4)
    pval_1 <- round(attr(art_mod, "Pvalue"), 20)

    # art_ <- ARTool::art(val ~ as.factor(grp), X_X_X)
    # aov_mod <- stats::anova(art_)
    # Fval_2 <- round(aov_mod$`F value`, 4)
    # pval_2 <- round(aov_mod$`Pr(>F)`, 20)

    testthat::expect_equal(Fval_1, 110.2735)
    testthat::expect_equal(pval_1, 0)

})
