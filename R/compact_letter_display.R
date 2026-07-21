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
#' @param display_null_letter Character (default: ""). Symbol for filling the letter's gap.
#' @param misc Logical (default: FALSE). Return other unimportant variables, not for users.
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
#'
#' @references
#' Piepho, H.-P. (2004).
#' An algorithm for a letter-based representation of all-pairwise comparisons.
#' Journal of Computational and Graphical Statistics, 13(2), 456–466.
#' https://doi.org/10.1198/1061860043515
#'
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
        display_null_letter = "",
        misc = FALSE
) {
    if (all(pvalues > alpha))
        return(stats::setNames(display_letters[1], grp_names))

    misc_lst <- list()

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

    misc_lst[["bool_mat_NULL"]] <- bool_mat

    for (i in seq_along(x1))
    {
        # The upper and lower triangle should be symmetric
        bool_mat[x1[i], x2[i]] <- bool[i]
        bool_mat[x2[i], x1[i]] <- bool[i]
    }

    misc_lst[["bool_mat_injected"]] <- bool_mat

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

    misc_lst[["bool_mat_absorbed"]] <- bool_mat

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

    misc_lst[["letter_mat"]] <- letter_mat

    #--------------------------------------------------------------------------#
    # Output:
    # The matrix will be reduced to a named-vector after row-wise collapsing.
    # The named-vector will also be resorted as the `grp_names` order.
    #--------------------------------------------------------------------------#
    ret <- apply(letter_mat, 1, function(x) paste(x, collapse = ""))
    ret <- ret[grp_names] # sort to the original `grp_names` order

    misc_lst[["cld"]] <- ret

    if (isTRUE(misc))
        return(misc_lst)
    else
        return(ret)
}


insert_absorb <- function(
        x1,
        x2,
        pvalues,
        grp_names,
        centers,
        alpha = 0.05,
        descending = TRUE,
        display_letters = base::letters,
        display_null_letter = "",
        misc = FALSE
) {
    if (all(pvalues > alpha))
        return(stats::setNames(display_letters[1], grp_names))

    misc_lst <- list()

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

    misc_lst[["bool_mat_NULL"]] <- bool_mat

    for (i in seq_along(x1))
    {
        # The upper and lower triangle should be symmetric
        bool_mat[x1[i], x2[i]] <- bool[i]
        bool_mat[x2[i], x1[i]] <- bool[i]
    }

    misc_lst[["bool_mat_injected"]] <- bool_mat

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

    misc_lst[["bool_mat_absorbed"]] <- bool_mat

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

    misc_lst[["letter_mat"]] <- letter_mat

    #--------------------------------------------------------------------------#
    # Output:
    # The matrix will be reduced to a named-vector after row-wise collapsing.
    # The named-vector will also be resorted as the `grp_names` order.
    #--------------------------------------------------------------------------#
    ret <- apply(letter_mat, 1, function(x) paste(x, collapse = ""))
    ret <- ret[grp_names] # sort to the original `grp_names` order

    misc_lst[["cld"]] <- ret

    if (isTRUE(misc))
        return(misc_lst)
    else
        return(ret)
}


insert_absorb_sweep <- function(
        x1,
        x2,
        pvalues,
        grp_names,
        centers,
        alpha = 0.05,
        descending = TRUE,
        display_letters = base::letters,
        display_null_letter = "",
        misc = FALSE
) {
    misc_lst <- insert_absorb(x1,
                              x2,
                              pvalues,
                              grp_names,
                              centers,
                              alpha,
                              descending,
                              display_letters,
                              display_null_letter,
                              misc = TRUE)
}




#' Convert p-values into asterisks
#'
#' Converts numeric p-values into categorical significance labels based on
#' user-defined break points. This function is commonly used for annotating
#' statistical significance levels in plots.
#'
#' The break points are sorted in decreasing order and define intervals for
#' assigning symbols. Missing values in `break_points` are removed before
#' classification.
#'
#' @param x A numeric vector of p-values.
#' @param break_points A numeric vector defining the boundaries for p-value
#'        categories. The default values correspond approximately to:
#'        \itemize{
#'          \item p > 0.055: "ns"
#'          \item 0.05 < p <= 0.055: "."
#'          \item 0.01 < p <= 0.05: one significance symbol
#'          \item 0.001 < p <= 0.01: two significance symbol
#'          \item smaller p-values: increasing numbers of significance symbols
#'        }
#' @param symbols A character vector containing the symbols used for each
#'        significance level. The first element represents non-significance,
#'        the second element represents the first significance level, and the
#'        third element is repeated for stronger significance levels.
#'
#' @return A character vector with the same length as `x`, containing the
#'         corresponding significance symbols for each p-value.
#'
#' @details
#' The function applies the following logic:
#' \itemize{
#'   \item p-values larger than the largest break point are assigned
#'   `symbols[1]`.
#'   \item Intermediate p-values are assigned symbols according to their
#'   corresponding intervals.
#'   \item For increasingly smaller p-values, `symbols[3]` is repeated to
#'   represent stronger significance levels.
#' }
#'
#' The function assumes that `break_points` and `symbols` are supplied in a
#' meaningful order. The number of significance levels supported depends on the
#' length of `break_points`.
#'
#' @examples
#' p <- c(0.2, 0.04, 0.008, 0.0005, 1e-6)
#' pval2asterisk(p)
#' @export
pval2asterisk <- function(
        x,
        break_points = c(0.055, 0.05, 0.01, 0.001, 0),
        symbols = c("ns", ".", "*")
) {
    bp <- break_points[stats::complete.cases(break_points)]
    bp <- sort(bp, decreasing = TRUE)
    n <- length(bp)

    symbols[grep("*", symbols)] <- "\U273D"

    vapply(
        x,
        function(pval)
        {
            if (pval > bp[1])
                return(symbols[1])
            if (pval < bp[1] & pval > bp[2])
                return(symbols[2])
            if (pval <= bp[2] & pval > bp[3])
                return(symbols[3])

            for (i in 3:(n - 1))
            {
                if (pval <= bp[i] & pval > bp[i + 1])
                    ret <- paste(rep(symbols[3], i - 1), collapse = "")
                else
                    next
            }
            return(ret)
        },
        FUN.VALUE = character(1)
    )
}
