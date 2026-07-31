tidy_to_list <- function(data, formula = NULL, factor_levels = NULL)
{
    # If data is a vector
    if (is.atomic(data) & is.null(dim(data)))
    {
        data <- data[stats::complete.cases(data)]
        lst <- list(data)
        names(lst) <- if (is.null(names(data))) "V1" else (names(data))
    }

    # If data is a matrix
    if (is.matrix(data))
        data <- as.data.frame(data)

    # If data is a data frame
    if (is.data.frame(data))
    {
        if (is.null(formula) || missing(formula))
            stop("`formula` must be specified, for example: y ~ x")

        all_vars <- all.vars(formula)
        y_name <- all_vars[1]
        x_name <- all_vars[2]
        if (length(all_vars) > 2)
        {
            warning(sprintf("Only the first independent variable (%s) is used.", x_name))
            formula <- sprintf("%s ~ %s", y_name, x_name)
            formula <- stats::as.formula(formula)
        }

        if (is.factor(data[[x_name]]) & is.null(factor_levels))
            factor_levels <- levels(data[[x_name]])

        df0 <- stats::model.frame(formula, data, drop.unused.levels = TRUE)
        data <- split(df0[, 1], df0[, 2])
    }

    # If data is a list
    # `is.null(dim(data))` is necessary as data frame is also a kind of list
    if (is.list(data) & is.null(dim(data)))
    {
        data <- lapply(data, function(x) x[stats::complete.cases(x)])
        n <- unlist(lapply(data, length), use.names = FALSE)

        if (any(n < 3))
            warning("All group sizes should be greater than 2.")

        if (is.null(names(data)))
            names(data) <- seq_along(data)

        lst <- data
    }

    if ( ! is.null(factor_levels) & ! missing(factor_levels) )
    {
        factor_levels <- as.character(factor_levels)
        if (all(names(lst) %in% factor_levels))
            lst <- lst[factor_levels]
        else
            warning("`factor_levels` doesn't match the input data factor levels.")
    }

    return(lst)
}


tidy_to_dataframe <- function(data, formula = NULL, factor_levels = NULL)
{
    # If data is a list
    # `is.null(dim(data))` is necessary as data frame is also a kind of list
    if (is.list(data) & is.null(dim(data)))
    {
        data <- lapply(data, function(x) x[stats::complete.cases(x)])
        data <- data
        isub <- seq_along(data)
        grp <- names(data)
        if (is.null(grp)) grp <- isub
        lst <- lapply(
            isub,
            function(i)
            {
                vct <- data[[i]]
                vct <- vct[stats::complete.cases(vct)]
                if (is.null(vct) || length(vct) == 0)
                    df0 <- data.frame(y = NA_real_, x = grp[i])
                else
                    df0 <- data.frame(y = vct, x = grp[i])
            }
        )
        df0 <- do.call(rbind.data.frame, lst)
        df0 <- df0[stats::complete.cases(df0[["y"]]), ]
        df0[["x"]] <- as.character(df0[["x"]])
        attr(df0, "x_name") <- "IV"
        attr(df0, "y_name") <- "DV"
    }

    # If data is a matrix
    if (is.matrix(data))
        data <- as.data.frame(data)

    # If data is a data frame
    if (is.data.frame(data))
    {
        if (missing(formula))
            stop("Please specify the `formula`.")

        df0 <- stats::model.frame(formula, data, drop.unused.levels = TRUE)
        x_name <- colnames(df0)[2]
        y_name <- colnames(df0)[1]
        colnames(df0) <- c("y", "x")

        if (is.factor(df0[["x"]]))
            factor_levels <- levels(df0[["x"]])

        df0[["x"]] <- as.character(df0[["x"]])
        df0 <- df0[stats::complete.cases(df0[["y"]]), ]
        attr(df0, "x_name") <- x_name
        attr(df0, "y_name") <- y_name
    }

    # Reorder the group names
    if ( ! is.null(factor_levels) & ! missing(factor_levels) )
    {
        factor_levels <- as.character(factor_levels)
        if ( ! all(unique(df0[["x"]]) %in% factor_levels) )
            warning("`factor_levels` doesn't match the input data factor levels.")
        ret <- df0[order(match(df0[["x"]], factor_levels)), ]
    } else {
        ret <- df0[order(df0[["x"]]), ]
    }

    return(ret)
}


#' Convert a data frame from wide to long format
#'
#' Reshapes a data frame from wide format to long format by stacking one or more columns into
#' a pair of key-value columns. This increases the number of rows while reducing the number of
#' columns. For more advanced reshaping operations, consider using `tidyr::pivot_longer()`.
#'
#' @param df A data frame.
#' @param columns A numeric or character vector specifying the columns to
#'        pivot into long format.
#' @param names_to A character string specifying the name of the new column
#'        containing the original column names. Default is `"grp"`.
#' @param values_to A character string specifying the name of the new column
#'        containing the values from the pivoted columns. Default is `"val"`.
#' @param keep Logical. If `TRUE`, columns not specified in `columns` are
#'        retained in the output. If `FALSE` (default), only the pivoted
#'        columns are returned.
#'
#' @returns
#' A data frame in long format. The output contains one column storing the
#' original column names (`names_to`) and another storing the corresponding
#' values (`values_to`). If `keep = TRUE`, non-pivoted columns are retained.
#'
#' @examples
#' n <- 10
#' df0 <- data.frame(
#'   G1 = stats::rnorm(n, 6, 1),
#'   G2 = stats::rnorm(n, 6, 1),
#'   G3 = stats::rnorm(n, 3, 1)
#' )
#'
#' df_wide_to_long(df0, c("G1", "G2"))
#'
#' @export
df_wide_to_long <- function(df, columns, names_to = "grp", values_to = "val", keep = FALSE)
{
    if (is.matrix(df))
        df <- as.data.frame(df)

    if ( ! is.data.frame(df) )
        stop("Input should be a data.frame.")

    default_rownames <- 1:(nrow(df) * length(columns))

    if (is.numeric(columns))
        grp_names <- colnames(df)[columns]
    else
        grp_names <- columns

    out <- stats::reshape(new.row.names = default_rownames,
                          data = df,
                          direction = "long",
                          timevar = names_to,
                          times = grp_names,
                          v.names = values_to,
                          varying = columns)

    if (isTRUE(keep))
        out <- out[, colnames(out) != "id"]
    else
        out <- out[, c(names_to, values_to)]

    return(out)
}

