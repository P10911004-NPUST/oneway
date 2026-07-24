test_that("oneway_anova", {

    # Classical one-way ANOVA
    aov_1 <- oneway_anova(plasma_etching, etch_rate ~ power, var_equal = TRUE, rounding = 5)
    aov_2 <- stats::oneway.test(etch_rate ~ power, plasma_etching, var.equal = TRUE)
    testthat::expect_equal(aov_1[["DF"]][1], aov_2[["parameter"]][["num df"]])
    testthat::expect_equal(aov_1[["DF"]][2], aov_2[["parameter"]][["denom df"]])
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Pvalue"]][1], 5), round(aov_2[["p.value"]], 5))

    # Welch's ANOVA
    aov_1 <- oneway_anova(anorexia, weight_gain ~ therapy, var_equal = FALSE, rounding = 5)
    aov_2 <- stats::oneway.test(weight_gain ~ therapy, anorexia, var.equal = FALSE)
    testthat::expect_equal(aov_1[["DF"]][1], aov_2[["parameter"]][["num df"]])
    testthat::expect_equal(aov_1[["DF"]][2], aov_2[["parameter"]][["denom df"]])
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Fvalue"]][1], 5), round(aov_2[["statistic"]][["F"]], 5))
    testthat::expect_equal(round(aov_1[["Pvalue"]][1], 5), round(aov_2[["p.value"]], 5))

})
