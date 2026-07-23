MS <- c(3, 5, 1, 8, 1, 1, 4, 9)
MM <- c(2, 12, 13, 6, 10, 7, 11, 19)
SS <- c(14, 6, 12, 4, 19, 3, 9, 21)
SM <- c(29, 20, 36, 21, 25, 18, 26, 17)
McM <- c(24, 26, 40, 32, 20, 33, 27, 30)

morphine <- data.frame(
    tolerance = c(MS, MM, SS, SM, McM),
    grp = rep(c("MS", "MM", "SS", "SM", "McM"), each = 8)
)

usethis::use_data(morphine, overwrite = TRUE)
