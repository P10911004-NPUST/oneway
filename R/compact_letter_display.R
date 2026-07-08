#' Compact Letter Display (CLD)
#'
#' Represent significance statements resulting from all-pairwise comparisons.
#'
#' @param x1 Character vector. The names of the minuend.
#' @param x2 Character vector. The names of the subtrahend.
#' @param pvalues Numeric vector. The p-values for the differences of each `x1 - x2`.
#' @param grp_names Character vector. The group names (each factor levels).
#' @param centers Numeric vector. Generally, the corresponding mean or median values of `grp_names`.
#' @param alpha Numeric (default: 0.05). Significance level, range from 0 to 1.
#' @param descending Logical (default: TRUE). If `TRUE`, sort the centers in decreasing order.
#' @param display_letters Character vector (default: `base::letters`). Display symbols.
#' @param display_null_letter Character (default: ""). Symbol for filling the letters' gap.
#'
#' @return A character vector.
#'
#' @examples
#' utils::data("iris", package = "datasets")
#' avg <- tapply(iris$Sepal.Length, iris$Species, "mean")
#' aov_mod <- stats::aov(Sepal.Length ~ Species, iris)
#' res <- stats::TukeyHSD(aov_mod)
#' res <- as.data.frame(res$Species)
#' res[["x1"]] <- unlist(lapply(strsplit(rownames(res), "-"), function(x) x[[1]]))
#' res[["x2"]] <- unlist(lapply(strsplit(rownames(res), "-"), function(x) x[[2]]))
#' compact_letter_display(
#'     x1 = res$x1,
#'     x2 = res$x2,
#'     pvalues = res$`p adj`,
#'     grp_names = names(avg),
#'     centers = unname(avg)
#' )
#' @references
#' Piepho, H.-P. (2004).
#' An algorithm for a letter-based representation of all-pairwise comparisons.
#' Journal of Computational and Graphical Statistics, 13(2), 456–466.
#' https://doi.org/10.1198/1061860043515
#' Piepho, H.-P. (2018).
#' Letters in mean comparisons: What they do and don't mean.
#' Agronomy Journal, 110(2), 431–434.
#' https://doi.org/10.2134/agronj2017.10.0580
#' @export
compact_letter_display <- function(
        x1,
        x2,
        pvalues,
        grp_names,
        centers,
        alpha = 0.05,
        descending = TRUE,
        display_letters = base::letters,
        display_null_letter = ""
) {
    n_grps <- length(grp_names)

    #--------------------------------------------------------------------------#
    # Ordered by the mean / median value of each groups
    #--------------------------------------------------------------------------#
    ind <- order(centers, decreasing = descending)
    g <- grp_names[ind]  # independent variable
    y <- centers[ind]    # response variable

    #--------------------------------------------------------------------------#
    # Insertion
    # Significantly different group-pairs will be annotated as `TRUE`
    # So, later, the letters will be inserted into the `FALSE` cells
    #--------------------------------------------------------------------------#
    bool <- pvalues < alpha
    bool_mat <- matrix(data = logical(n_grps * n_grps),
                       nrow = n_grps,
                       ncol = n_grps,
                       dimnames = list(g, g))

    for (i in seq_along(x1))
    {
        # The upper and lower triangle should be symmetric
        bool_mat[x1[i], x2[i]] <- bool[i]
        bool_mat[x2[i], x1[i]] <- bool[i]
    }

    #--------------------------------------------------------------------------#
    # Absorption
    # Remove duplicated columns
    #--------------------------------------------------------------------------#
    redundant_col <- vector("logical", n_grps)
    for (i in 1:n_grps)
    {
        if (i == 1)
        {
            # keep the first row, it is not redundant
            redundant_col[i] <- FALSE
            next
        }

        col_head <- bool_mat[, i, drop = TRUE]
        col_tail <- bool_mat[, 1:(i - 1), drop = FALSE]
        is_redundant <- apply(col_tail, 2, function(x) identical(x, col_head))
        redundant_col[i] <- any(is_redundant)
    }

    bool_mat <- bool_mat[, !redundant_col, drop = FALSE]
    bool_mat[upper.tri(bool_mat)] <- TRUE  # avoid injecting letters to duplicated pairs

    letter_mat <- bool_mat
    for (i in 1:ncol(bool_mat))
    {
        letter_mat[, i] <- vapply(bool_mat[, i, drop = TRUE],
                                  function(x)
                                  {
                                      if (isFALSE(x))
                                          return(display_letters[i])
                                      else
                                          return(display_null_letter)
                                  },
                                  FUN.VALUE = character(1))
    }

    #--------------------------------------------------------------------------#
    # Output:
    # The matrix will be reduced to a named-vector after row-wise collapsing.
    # The named-vector will also be resorted as the `grp_names` order.
    #--------------------------------------------------------------------------#
    ret <- apply(letter_mat, 1, function(x) paste(x, collapse = ""))
    ret <- ret[grp_names] # sort to the original `grp_names` order
    return(ret)
}



