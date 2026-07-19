plasma_etching <- data.frame(
    "power"     = as.factor(rep(c(160, 180, 200, 220), each = 5)),
    "etch_rate" = c(575, 542, 530, 539, 570,
                    565, 593, 590, 579, 610,
                    600, 651, 610, 637, 629,
                    725, 700, 715, 685, 710)
)

usethis::use_data(plasma_etching, overwrite = TRUE)
