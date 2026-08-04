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

    # -------------------------------------------------------------------------- #
    # Ordered by the mean / median value of each groups
    # -------------------------------------------------------------------------- #
    ind <- order(centers, decreasing = descending)
    g <- grp_names[ind]  # independent variable
    y <- centers[ind]    # response variable

    # -------------------------------------------------------------------------- #
    # Insertion
    # Significantly different group-pairs will be annotated as `TRUE`
    # So, later, the letters will be inserted into the `FALSE` cells
    # -------------------------------------------------------------------------- #
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

    # -------------------------------------------------------------------------- #
    # Absorption
    # Remove duplicated columns
    # -------------------------------------------------------------------------- #
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

    # -------------------------------------------------------------------------- #
    # Output:
    # The matrix will be reduced to a named-vector after row-wise collapsing.
    # The named-vector will also be resorted as the `grp_names` order.
    # -------------------------------------------------------------------------- #
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

    # -------------------------------------------------------------------------- #
    # Ordered by the mean / median value of each groups
    # -------------------------------------------------------------------------- #
    ind <- order(centers, decreasing = descending)
    g <- grp_names[ind]  # independent variable
    y <- centers[ind]    # response variable

    # -------------------------------------------------------------------------- #
    # Insertion
    # Significantly different group-pairs will be annotated as `TRUE`
    # So, later, the letters will be inserted into the `FALSE` cells
    # -------------------------------------------------------------------------- #
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

    # -------------------------------------------------------------------------- #
    # Absorption
    # Remove duplicated columns
    # -------------------------------------------------------------------------- #
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

    # -------------------------------------------------------------------------- #
    # Output:
    # The matrix will be reduced to a named-vector after row-wise collapsing.
    # The named-vector will also be resorted as the `grp_names` order.
    # -------------------------------------------------------------------------- #
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




#' Convert p-values to significance labels
#'
#' Converts numeric p-values into categorical significance labels according to user-defined
#' thresholds. The function is commonly used to annotate statistical results in tables and figures.
#'
#' @param x A numeric vector of p-values.
#' @param break_points A numeric vector of significance thresholds in
#'        descending order. Each threshold defines the upper bound of a significance
#'        interval. The default values correspond to:
#'        \itemize{
#'          \item \eqn{p > 0.055}: `"ns"`
#'          \item \eqn{0.05 < p \le 0.055}: `"."`
#'          \item \eqn{0.01 < p \le 0.05}: `"*"`
#'          \item \eqn{0.001 < p \le 0.01}: `"**"`
#'          \item \eqn{p \le 0.001}: `"***"`
#'        }
#' @param symbols A character vector of significance labels corresponding to `break_points`.
#'        The lengths of `break_points` and `symbols` must be identical.
#'
#' @returns
#' A character vector of the same length as `x`, where each element is the corresponding
#' significance label.
#'
#' @details
#' Each p-value is assigned to exactly one interval defined by `break_points`. Values greater
#' than the first threshold are assigned `symbols[1]`, whereas values less than or equal to the
#' last threshold are assigned the last element of `symbols`. Intermediate intervals are matched
#' sequentially.
#'
#' The function assumes that `break_points` are supplied in descending order.
#'
#' @examples
#' p <- c(0.20, 0.04, 0.008, 0.0005, 1e-6)
#' pval2asterisk(p)
#'
#' # Custom significance labels
#' pval2asterisk(
#'   p,
#'   break_points = c(0.05, 0.01, 0),
#'   symbols = c("Not significant", "Significant", "Highly significant")
#' )
#'
#' @export
pval2asterisk <- function(
        x,
        break_points = c(0.055, 0.05, 0.01, 0.001, 0),
        symbols = c("ns", ".", "*", "**", "***")
) {
    if (length(break_points) != length(symbols))
        stop("Length of `break_points` and `symbols` should be identical.")

    n <- length(break_points)

    # symbols[grep("*", symbols)] <- "\U273D"

    vapply(
        x,
        function(pval)
        {
            if (pval > break_points[1]) return(symbols[1])
            if (pval <= break_points[n]) return(symbols[n])
            for (i in 2:n)
                if (pval > break_points[i] & pval <= break_points[i - 1]) return(symbols[i])
        },
        FUN.VALUE = character(1)
    )
}
