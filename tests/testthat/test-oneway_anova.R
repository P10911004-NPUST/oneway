test_that("oneway_anova", {

    # Classical one-way ANOVA
    aov_1 <- oneway_anova(O_O_O, val ~ grp, var_equal = TRUE, rounding = 5, silent = TRUE)
    aov_2 <- stats::oneway.test(val ~ grp, O_O_O, var.equal = TRUE)
    testthat::expect_equal(aov_1[["DF"]][1], aov_2[["parameter"]][["num df"]])
    testthat::expect_equal(aov_1[["DF"]][2], aov_2[["parameter"]][["denom df"]])
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Pvalue"]][1], 5), round(aov_2[["p.value"]], 5))

    aov_1 <- oneway_anova(O_O_X, val ~ grp, var_equal = TRUE, rounding = 5, silent = TRUE)
    aov_2 <- stats::oneway.test(val ~ grp, O_O_X, var.equal = TRUE)
    testthat::expect_equal(aov_1[["DF"]][1], aov_2[["parameter"]][["num df"]])
    testthat::expect_equal(aov_1[["DF"]][2], aov_2[["parameter"]][["denom df"]])
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Pvalue"]][1], 5), round(aov_2[["p.value"]], 5))

    # Welch's ANOVA
    aov_1 <- oneway_anova(O_X_X, val ~ grp, var_equal = FALSE, rounding = 5, silent = TRUE)
    aov_2 <- stats::oneway.test(val ~ grp, O_X_X, var.equal = FALSE)
    testthat::expect_equal(round(aov_1[["DF"]][1], 5), round(aov_2[["parameter"]][["num df"]], 5))
    testthat::expect_equal(round(aov_1[["DF"]][2], 5), round(aov_2[["parameter"]][["denom df"]], 5))
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Pvalue"]][1], 5), round(aov_2[["p.value"]], 5))

})
