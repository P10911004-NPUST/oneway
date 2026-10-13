test_that("Scheffe_test", {
    lst0 <- list(g1 = c(6.9,  5.4, 5.8, 4.6, 4.0),
                 g2 = c(8.3,  6.8, 7.8, 9.2, 6.5),
                 g3 = c(8.0, 10.5, 8.1, 6.9, 9.3),
                 g4 = c(5.8,  3.8, 6.1, 5.6, 6.2))

    c1 <- c(1, 1, -1, -1)
    c2 <- c(1, -1, 1, -1)
    contr <- matrix(c(c1, c2), nrow = 2, byrow = TRUE)
    colnames(contr) <- paste0("g", 1:4)

    out <- Scheffe_test(lst0, planned_contrasts = contr, rounding = 3, verbose = FALSE)

    c1_CI <- out$post_hoc$`[x1 - x2] 95% CI`[1]
    c1_lower_CI <- as.numeric(gsub("^\\[\\s*([^,]+),.*\\]$", "\\1", c1_CI))
    c1_upper_CI <- as.numeric(gsub("^\\[.*,\\s*([^]]+)\\]$", "\\1", c1_CI))

    testthat::expect_equal(c1_lower_CI, -2.108)
    testthat::expect_equal(c1_upper_CI, 1.108)

    c2_CI <- out$post_hoc$`[x1 - x2] 95% CI`[2]
    c2_lower_CI <- as.numeric(gsub("^\\[\\s*([^,]+),.*\\]$", "\\1", c2_CI))
    c2_upper_CI <- as.numeric(gsub("^\\[.*,\\s*([^]]+)\\]$", "\\1", c2_CI))

    testthat::expect_equal(c2_lower_CI, -1.268)
    testthat::expect_equal(c2_upper_CI, 1.948)
})
