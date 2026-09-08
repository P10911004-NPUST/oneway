test_that("pairwise_comparison", {

    out <- pairwise_comparison(O_O_O, val ~ grp, verbose = FALSE)
    print(out$summary)
    boxplot(val ~ grp, O_O_O)

    out <- pairwise_comparison(O_X_X, val ~ grp, verbose = FALSE)
    print(row_arrange(out$summary, "MED"))
    boxplot(val ~ grp, O_X_X)

    out <- pairwise_comparison(X_X_X, val ~ grp, verbose = FALSE)
    print(row_arrange(out$summary, "MED"))
    boxplot(val ~ grp, X_X_X)

})
