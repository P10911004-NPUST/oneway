tidy_to_list <- function(data, formula = NULL, factor_levels = NULL)
{
    # If data is a vector
    if (is.atomic(data) & is.null(dim(data)))
    {
        data <- data[stats::complete.cases(data)]
        lst <- list(data)
        names(lst) <- names(data)
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
        ret <- do.call(rbind.data.frame, lst)
        ret <- ret[stats::complete.cases(ret[["y"]]), ]
        ret[["x"]] <- as.character(ret[["x"]])
        attr(ret, "x_name") <- "IV"
        attr(ret, "y_name") <- "DV"
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
        ret <- df0[stats::complete.cases(df0[["y"]]), ]
        attr(ret, "x_name") <- x_name
        attr(ret, "y_name") <- y_name
    }

    if ( ! is.null(factor_levels) & ! missing(factor_levels) )
    {
        factor_levels <- as.character(factor_levels)
        if ( ! all(unique(df0[["x"]]) %in% factor_levels) )
            warning("`factor_levels` doesn't match the input data factor levels.")
        ret <- ret[order(match(df0[["x"]], factor_levels)), ]
    } else {
        ret <- ret[order(ret[["x"]]), ]
    }

    return(ret)
}

