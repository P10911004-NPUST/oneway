# ------------------------------------------------------------------------------------------- #
#               O_O_O: Normally distributed, Homoscedastic, Balance-designed
# ------------------------------------------------------------------------------------------- #
set.seed(123)

n <- 30
O_O_O <- stats::reshape(
    new.row.names = 1:(6 * n),
    data = data.frame(
        G1 = stats::rnorm(n, 6, 1),
        G2 = stats::rnorm(n, 6, 1),
        G3 = stats::rnorm(n, 3, 1),
        G4 = stats::rnorm(n, 5, 1),
        G5 = stats::rnorm(n, 2, 1),
        G6 = stats::rnorm(n, 4, 1)
    ),
    direction = "long",
    v.names = "val",
    # varying = paste0("G", 1:5),
    varying = 1:5,
    timevar = "grp",
    times = paste0("G", 1:5)
)[, c("grp", "val")]

use_data(O_O_O, overwrite = TRUE)
