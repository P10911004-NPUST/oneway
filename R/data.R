#' Simulated datasets for statistical analysis
#'
#' A collection of six simulated datasets representing different combinations
#' of distributional assumptions, variance homogeneity, and sample-size
#' balance. These datasets are designed for demonstrating and evaluating
#' statistical procedures under a range of common experimental conditions.
#'
#' All datasets contain six groups (`G1` to `G6`) and two variables:
#' \describe{
#'   \item{`grp`}{A character variable identifying the group.}
#'   \item{`val`}{A numeric response variable.}
#' }
#'
#' The dataset names follow a three-character notation separated by
#' underscores: `D_V_B`, where each position describes a different property
#' of the data:
#' \describe{
#'   \item{First position (`D`)}{Distribution. `O` indicates normally
#'   distributed data, whereas `X` indicates distribution-free or
#'   non-normally distributed data.}
#'   \item{Second position (`V`)}{Variance. `O` indicates homoscedasticity,
#'   whereas `X` indicates heteroscedasticity.}
#'   \item{Third position (`B`)}{Design balance. `O` indicates a balanced
#'   design, whereas `X` indicates an unbalanced design.}
#' }
#'
#' The six datasets are:
#' \describe{
#'   \item{`O_O_O`}{Normally distributed, homoscedastic, and
#'   balanced-designed data. Each group contains 20 observations.}
#'   \item{`O_O_X`}{Normally distributed, homoscedastic, and
#'   unbalanced-designed data. The group sample sizes are 27, 24, 16, 30,
#'   11, and 20, respectively.}
#'   \item{`O_X_X`}{Normally distributed, heteroscedastic, and
#'   unbalanced-designed data. The group sample sizes are 27, 24, 16, 30,
#'   11, and 20, respectively.}
#'   \item{`X_O_O`}{Distribution-free or non-normally distributed,
#'   homoscedastic, and balanced-designed data. Each group contains
#'   20 observations.}
#'   \item{`X_X_O`}{Distribution-free or non-normally distributed,
#'   heteroscedastic, and balanced-designed data. Each group contains
#'   20 observations.}
#'   \item{`X_X_X`}{Distribution-free or non-normally distributed,
#'   heteroscedastic, and unbalanced-designed data. The group sample sizes
#'   are 27, 24, 16, 30, 11, and 20, respectively.}
#' }
#'
#' The normally distributed datasets were generated using
#' [stats::rnorm()]. The distribution-free datasets were generated using
#' combinations of [stats::rcauchy()], [stats::runif()], and
#' [stats::rgamma()], together with [stats::rnorm()]. Different location and
#' scale parameters were used to produce the intended distributional and
#' variance characteristics.
#'
#' These datasets are intended for methodological examples, unit tests,
#' demonstrations, and comparisons of statistical procedures under different
#' combinations of assumptions. They should not be interpreted as empirical
#' observations from a real population.
#'
#' @format
#' Six data frames, each containing the following two variables:
#' \describe{
#'   \item{`grp`}{Character vector identifying the experimental group.}
#'   \item{`val`}{Numeric vector containing the simulated response values.}
#' }
#'
#' The number of observations differs according to the experimental design:
#' balanced datasets contain 120 observations (20 per group), whereas
#' unbalanced datasets contain 128 observations with group sizes of
#' 27, 24, 16, 30, 11, and 20.
#'
#' @section Dataset characteristics:
#'
#' \tabular{llll}{
#'   Dataset \tab Distribution \tab Variance \tab Design \cr
#'   `O_O_O` \tab Normal \tab Homoscedastic \tab Balanced \cr
#'   `O_O_X` \tab Normal \tab Homoscedastic \tab Unbalanced \cr
#'   `O_X_X` \tab Normal \tab Heteroscedastic \tab Unbalanced \cr
#'   `X_O_O` \tab Non-normal \tab Homoscedastic \tab Balanced \cr
#'   `X_X_O` \tab Non-normal \tab Heteroscedastic \tab Balanced \cr
#'   `X_X_X` \tab Non-normal \tab Heteroscedastic \tab Unbalanced
#' }
#'
#' @examples
#' data(O_O_O)
#'
#' boxplot(val ~ grp, data = O_O_O)
#'
#' data(X_X_X)
#'
#' boxplot(val ~ grp, data = X_X_X)
#'
#' aggregate(val ~ grp, data = O_O_O, FUN = mean)
#'
#' aggregate(val ~ grp, data = X_X_X, FUN = mean)
#'
#' @name simulated_data
#' @aliases O_O_O O_O_X O_X_X X_O_O X_X_O X_X_X
#' @docType data
NULL


#' @rdname simulated_data
"O_O_O"
#' @rdname simulated_data
"O_O_X"
#' @rdname simulated_data
"O_X_X"
#' @rdname simulated_data
"X_O_O"
#' @rdname simulated_data
"X_X_O"
#' @rdname simulated_data
"X_X_X"


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


