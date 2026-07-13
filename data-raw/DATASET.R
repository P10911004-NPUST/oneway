# Normally distributed, Homoscedastic, Balance-designed
O_O_O <- data.frame(
    G1 = stats::rnorm(20, 6, 1),
    G2 = stats::rnorm(20, 6, 1),
    G3 = stats::rnorm(20, 3, 1),
    G4 = stats::rnorm(20, 5, 1),
    G5 = stats::rnorm(20, 2, 1),
    G6 = stats::rnorm(20, 4, 1)
)

O_O_O <- stats::reshape(
    new.row.names = 1:120,
    data = data.frame(
        G1 = stats::rnorm(20, 6, 1),
        G2 = stats::rnorm(20, 6, 1),
        G3 = stats::rnorm(20, 3, 1),
        G4 = stats::rnorm(20, 5, 1),
        G5 = stats::rnorm(20, 2, 1),
        G6 = stats::rnorm(20, 4, 1)
    ),
    direction = "long",
    v.names = "val",
    varying = colnames(O_O_O),
    timevar = "grp",
    times = colnames(O_O_O)
)[, c("grp", "val")]

use_data(O_O_O, overwrite = TRUE)
