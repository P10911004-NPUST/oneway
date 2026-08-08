# ------------------------------------------------------------------------------------------- #
#               O_O_O: Normally distributed, Homoscedastic, Balanced-designed
# ------------------------------------------------------------------------------------------- #
set.seed(123)

# n <- 20
# O_O_O <- stats::reshape(
#     new.row.names = 1:(6 * n),
#     data = data.frame(
#         G1 = stats::rnorm(n, 4.5, 1),
#         G2 = stats::rnorm(n, 6, 1),
#         G3 = stats::rnorm(n, 3, 1),
#         G4 = stats::rnorm(n, 5, 1),
#         G5 = stats::rnorm(n, 6, 1),
#         G6 = stats::rnorm(n, 4, 1)
#     ),
#     direction = "long",
#     v.names = "val",
#     # varying = paste0("G", 1:5),
#     varying = 1:5,
#     timevar = "grp",
#     times = paste0("G", 1:5)
# )[, c("grp", "val")]
# utils::write.csv(O_O_O, "./data-raw/O_O_O.csv", row.names = FALSE)
O_O_O <- utils::read.csv("./data-raw/O_O_O.csv")
usethis::use_data(O_O_O, overwrite = TRUE)


# ------------------------------------------------------------------------------------------- #
#               O_O_X: Normally distributed, Homoscedastic, Unbalanced-designed
# ------------------------------------------------------------------------------------------- #
# n <- c(27, 24, 16, 30, 11, 20)
# lst <- list(
#     data.frame("grp" = "G1", "val" = stats::rnorm(n[1], 6, 1)),
#     data.frame("grp" = "G2", "val" = stats::rnorm(n[2], 6, 1)),
#     data.frame("grp" = "G3", "val" = stats::rnorm(n[3], 3, 1)),
#     data.frame("grp" = "G4", "val" = stats::rnorm(n[4], 5, 1)),
#     data.frame("grp" = "G5", "val" = stats::rnorm(n[5], 2, 1)),
#     data.frame("grp" = "G6", "val" = stats::rnorm(n[6], 4, 1))
# )
# O_O_X <- do.call(rbind.data.frame, lst)
# utils::write.csv(O_O_X, "./data-raw/O_O_X.csv", row.names = FALSE)
O_O_X <- utils::read.csv("./data-raw/O_O_X.csv")
usethis::use_data(O_O_X, overwrite = TRUE)



# ------------------------------------------------------------------------------------------- #
#           O_X_X: Normally distributed, Heteroscedastic, Unbalanced-designed
# ------------------------------------------------------------------------------------------- #
# n <- c(27, 24, 16, 30, 11, 20)
# lst <- list(
#     data.frame("grp" = "G1", "val" = stats::rnorm(n[1], 16, 2)),
#     data.frame("grp" = "G2", "val" = stats::rnorm(n[2], 10, 3)),
#     data.frame("grp" = "G3", "val" = stats::rnorm(n[3],  9, 2)),
#     data.frame("grp" = "G4", "val" = stats::rnorm(n[4],  5, 1)),
#     data.frame("grp" = "G5", "val" = stats::rnorm(n[5], 12, 4)),
#     data.frame("grp" = "G6", "val" = stats::rnorm(n[6], 14, 1))
# )
# O_X_X <- do.call(rbind.data.frame, lst)
# utils::write.csv(O_X_X, "./data-raw/O_X_X.csv", row.names = FALSE)
O_X_X <- utils::read.csv("./data-raw/O_X_X.csv")
usethis::use_data(O_X_X, overwrite = TRUE)



# ------------------------------------------------------------------------------------------- #
#           X_O_0: Distribution-free, Homoscedastic, Balanced-designed
# ------------------------------------------------------------------------------------------- #
# n <- rep(20, 6)
# lst <- list(
#     data.frame("grp" = "G1", "val" = stats::rcauchy(n[1], 12, 0.2)),
#     data.frame("grp" = "G2", "val" = stats::rnorm(n[5], 5, 1)),
#     data.frame("grp" = "G3", "val" = stats::rgamma(n[3], 1) + 5),
#     data.frame("grp" = "G4", "val" = stats::rgamma(n[3], 2) + 8),
#     data.frame("grp" = "G5", "val" = stats::rnorm(n[5], 10, 1)),
#     data.frame("grp" = "G6", "val" = stats::rcauchy(n[1], 7, 0.2))
# )
# X_O_O <- do.call(rbind.data.frame, lst)
# boxplot(val ~ grp, X_O_O)
# normality::is_normal(X_O_O, val ~ grp)
# varequal::is_var_equal(X_O_O, val ~ grp)
# utils::write.csv(X_O_O, "./data-raw/X_O_O.csv", row.names = FALSE)
X_O_O <- utils::read.csv("./data-raw/X_O_O.csv")
usethis::use_data(X_O_O, overwrite = TRUE)



# ------------------------------------------------------------------------------------------- #
#           X_X_0: Distribution-free, Heteroscedastic, Balanced-designed
# ------------------------------------------------------------------------------------------- #
# n <- rep(20, 6)
# lst <- list(
#     data.frame("grp" = "G1", "val" = stats::rcauchy(n[1], 12, 0.2)),
#     data.frame("grp" = "G2", "val" = stats::runif(n[2], 3, 12)),
#     data.frame("grp" = "G3", "val" = stats::rgamma(n[3], 1) + 5),
#     data.frame("grp" = "G4", "val" = stats::rgamma(n[3], 2) + 8),
#     data.frame("grp" = "G5", "val" = stats::rnorm(n[5], 10, 2)),
#     data.frame("grp" = "G6", "val" = stats::rcauchy(n[1], 7, 1))
# )
# X_X_O <- do.call(rbind.data.frame, lst)
# boxplot(val ~ grp, X_X_O)
# normality::is_normal(X_X_O, val ~ grp)
# varequal::is_var_equal(X_X_O, val ~ grp)
# utils::write.csv(X_X_O, "./data-raw/X_X_O.csv", row.names = FALSE)
X_X_O <- utils::read.csv("./data-raw/X_X_O.csv")
usethis::use_data(X_X_O, overwrite = TRUE)



# ------------------------------------------------------------------------------------------- #
#           X_X_X: Distribution-free, Heteroscedastic, Unbalanced-designed
# ------------------------------------------------------------------------------------------- #
# n <- c(27, 24, 16, 30, 11, 20)
# lst <- list(
#     data.frame("grp" = "G1", "val" = stats::rcauchy(n[1], 10, 0.5)),
#     data.frame("grp" = "G2", "val" = stats::runif(n[2], 1, 9)),
#     data.frame("grp" = "G3", "val" = stats::rgamma(n[3], 1) + 1),
#     data.frame("grp" = "G4", "val" = stats::rgamma(n[3], 2) + 5),
#     data.frame("grp" = "G5", "val" = stats::rnorm(n[5], 12, 3)),
#     data.frame("grp" = "G6", "val" = stats::rgamma(n[3], 3) + 1)
# )
# X_X_X <- do.call(rbind.data.frame, lst)
# boxplot(val ~ grp, X_X_X)
# normality::is_normal(X_X_X, val ~ grp)
# varequal::is_var_equal(X_X_X, val ~ grp)
# is_balance(X_X_X, val ~ grp, 0.2)
# utils::write.csv(X_X_X, "./data-raw/X_X_X.csv", row.names = FALSE)
X_X_X <- utils::read.csv("./data-raw/X_X_X.csv")
usethis::use_data(X_X_X, overwrite = TRUE)
