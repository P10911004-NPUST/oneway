# Effective Therapies for Anorexia (Chapter 11, p. 342)
# Table 11.5 Data from Everitt on the treatment of anorexia in young girls
# Howell, D. C. (2013). Statistical methods for psychology (8th ed.). Cengage.

control <- c(-0.5, -9.3, -5.4, 12.3, -2, -10.2, -12.2, 11.6, -7.1, 6.2,
             -0.2, -9.2, 8.3, 3.3, 11.3, 0, -1, -10.6, -4.6, -6.7, 2.8,
             0.3, 1.8, 3.7, 15.9, -10.2)
cognitive <- c(1.7, 0.7, -0.1, -0.7, -3.5, 14.9, 3.5, 17.1, -7.6, 1.6,
               11.7, 6.1, 1.1, -4.0, 20.9, -9.1, 2.1, -1.4, 1.4, -0.3,
               -3.7, -0.8, 2.4, 12.6, 1.9, 3.9, 0.1, 15.4, -0.7)
family <- c(11.4, 11.0, 5.5, 9.4, 13.6, -2.9, -0.1, 7.4, 21.5, -5.3,
            -3.8, 13.4, 13.1, 9.0, 3.9, 5.7, 10.7)

anorexia <- data.frame(
    therapy = c(rep("control", length(control)),
                rep("cognitive", length(cognitive)),
                rep("family", length(family))),
    weight_gain = c(control, cognitive, family)
)

usethis::use_data(anorexia, overwrite = TRUE)
