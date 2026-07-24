#' Simulated normally distributed data with homogeneous variances and balanced design
#'
#' A simulated dataset representing a one-way experimental design with six
#' independent groups. The dataset was generated under the different assumptions
#' of normality, homogeneity of variance, and equal sample sizes among groups.
#'
#' @format A data frame with 120 rows and 2 columns:
#' \describe{
#'   \item{grp}{A factor identifying the experimental group (G1--G6).}
#'   \item{val}{A numeric response variable.}
#' }
#' @name simulated_datasets
NULL


#' @rdname simulated_datasets
"O_O_O"


#' Plasma Etching Experiment
#'
#' Etch Rate Data (in Å/min) from the Plasma Etching Experiment.
#'
#' @format A data frame with 20 rows and 2 columns:
#' \describe{
#'   \item{power}{A factor identifying the power (W) (160(20)220).}
#'   \item{etch_rate}{A numeric response variable.}
#' }
"plasma_etching"


#' Weight Gain Following Therapy for Anorexia
#'
#' A dataset containing weight gain for young girls with anorexia following
#' three different treatment conditions: a control group, cognitive-behavior
#' therapy, and family therapy. The response variable is weight gain measured
#' after treatment.
#'
#' @format A data frame with 72 observations and 2 variables:
#' \describe{
#'   \item{therapy}{A factor indicating the treatment group with three levels:
#'   `"control"`, `"cognitive"`, and `"family"`.}
#'   \item{weight_gain}{A numeric vector giving the observed weight gain after
#'   treatment.}
#' }
#'
#' @details
#' This dataset is commonly used to illustrate one-way analysis of variance
#' (ANOVA) and multiple comparison procedures. The observations were originally
#' reported by Everitt and reproduced by Howell (2013).
#'
#' Group sample sizes are:
#' \itemize{
#'   \item Control: 26
#'   \item Cognitive-behavior therapy: 29
#'   \item Family therapy: 17
#' }
#'
#' @references
#' Howell, D. C. (2013). Statistical methods for psychology (8th ed.).
#' Cengage. Chapter 11, Table 11.5, pg. 342.
"anorexia"


#' Morphine tolerance data
#'
#' A dataset containing morphine tolerance measurements from five experimental
#' treatment groups. The data consist of tolerance values recorded for eight
#' experimental units in each group, resulting in a balanced one-way design with
#' 40 observations.
#'
#' The treatment groups are coded as:
#' \describe{
#'   \item{MS}{Morphine followed by saline.}
#'   \item{MM}{Morphine followed by morphine.}
#'   \item{SS}{Saline followed by saline.}
#'   \item{SM}{Saline followed by morphine.}
#'   \item{McM}{Morphine followed by challenge morphine.}
#' }
#'
#' @format A data frame with 40 rows and 2 columns:
#' \describe{
#'   \item{tolerance}{A numeric variable containing the morphine tolerance
#'   measurement.}
#'   \item{grp}{A factor identifying the experimental treatment group with five
#'   levels: `"MS"`, `"MM"`, `"SS"`, `"SM"`, and `"McM"`.}
#' }
#'
#' @details
#' The dataset represents a balanced one-way experimental design with five
#' independent groups and eight observations per group. It is suitable for
#' demonstrating one-way ANOVA, nonparametric alternatives, homogeneity of
#' variance tests, post hoc comparisons, and effect size estimation.
#'
#' @references
#' Howell, D. C. (2013). Statistical methods for psychology (8th ed.).
#' Cengage. Chapter 11, Table 12.1, pg. 375.
"morphine"
