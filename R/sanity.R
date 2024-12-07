usepackage_latex <- function(name, options = NULL, extra_lines = NULL) {
    assert_dependency("rmarkdown")
    invisible(knitr::knit_meta_add(list(rmarkdown::latex_dependency(name, options, extra_lines))))
}


sanity_align <- function(align, i) {
    if (any(grepl("d", align)) && !is.null(i)) {
        msg <- "d column alignment can only be applied to entire columns. `i` must be `NULL`."
        stop(msg, call. = FALSE)
    }
}


sanitize_i <- function(i, x, pre_group_i = FALSE, lazy = TRUE) {
    if (identical(i, "notes")) {
        return(i)
    }
    out <- seq_len(nrow(x))
    assert_numeric(i, null.ok = TRUE, name = "i")
    if (is.null(i) && isTRUE(lazy)) {
        out <- NA
        attr(out, "null") <- TRUE
        attr(out, "body") <- seq_len(nrow(x))
        attr(out, "head") <- integer()
    } else {
        if (!is.null(i)) {
            out <- i
        } else if (inherits(x, "tinytable")) {
            out <- seq_len(nrow(x@table_dataframe))
        }
        attr(out, "null") <- FALSE
        attr(out, "body") <- out[out > 0]
        attr(out, "head") <- out[out < 1]
    }
    return(out)
}


sanitize_j <- function(j, x) {
    # regex
    if (is.character(j) && length(j) == 1 && !is.null(colnames(x))) {
        out <- grep(j, colnames(x), perl = TRUE)
        # full names
    } else if (is.character(j) && length(j) > 1 && !is.null(colnames(x))) {
        bad <- setdiff(j, colnames(x))
        if (length(bad) > 0) {
            msg <- sprintf("Missing columns: %s", paste(bad, collapse = ", "))
            stop(msg, call. = FALSE)
        }
        out <- which(colnames(x) %in% j)
    } else {
        assert_integerish(j, lower = 1, upper = ncol(x), null.ok = TRUE)
        if (is.null(j)) {
            out <- seq_len(ncol(x))
        } else {
            out <- j
        }
    }
    attr(out, "null") <- is.null(j)
    return(out)
}

sanitize_output <- function(output) {
    assert_choice(output, choice = c("tinytable", "markdown", "latex", "html", "typst", "dataframe", "gfm"), null.ok = TRUE)

    # default output format
    if (is.null(output) || isTRUE(output == "tinytable")) {
        has_viewer <- interactive() && !is.null(getOption("viewer"))
        out <- if (has_viewer) "html" else "markdown"
    } else {
        out <- output
    }

    if (isTRUE(check_dependency("knitr"))) {
        if (isTRUE(knitr::pandoc_to() %in% c("latex", "beamer"))) {
            flag <- getOption("tinytable_latex_preamble", default = TRUE)
            if (isTRUE(flag)) {
                usepackage_latex("float")
                usepackage_latex("tabularray", extra_lines = c(
                    "\\usepackage[normalem]{ulem}",
                    "\\usepackage{graphicx}",
                    "\\UseTblrLibrary{booktabs}",
                    "\\UseTblrLibrary{rotating}",
                    "\\UseTblrLibrary{siunitx}",
                    "\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}",
                    "\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}",
                    "\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}"
                ))
            }
            if (is.null(output)) out <- "latex"
        } else if (isTRUE(knitr::pandoc_to() %in% c("html", "revealjs"))) {
            if (is.null(output)) out <- "html"
        } else if (isTRUE(knitr::pandoc_to() == "typst")) {
            if (is.null(output)) out <- "typst"
            if (isTRUE(check_dependency("quarto"))) {
                if (isTRUE(quarto::quarto_version() < "1.5.29")) {
                    msg <- "Typst tables require version 1.5.29 or later of Quarto and version 0.11.0 or later of Typst. This software may (or may not) only be available in pre-release builds: https://quarto.org/docs/download"
                    stop(msg, call. = FALSE)
                }
            }
        } else if (isTRUE(knitr::pandoc_to() == "docx")) {
            if (is.null(output)) out <- "markdown"
        } else {
            if (is.null(output)) out <- "markdown"
        }
    }

    return(out)
}


#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
    flag <- requireNamespace(library_name, quietly = TRUE)
    if (isFALSE(flag)) {
        msg <- sprintf("Please install the `%s` package.", library_name)
        return(msg)
    } else {
        return(TRUE)
    }
}

assert_dependency <- function(library_name) {
    flag <- check_dependency(library_name)
    if (!isTRUE(flag)) stop(flag, call. = FALSE)
    return(invisible())
}

assert_choice <- function(x, choice, null.ok = FALSE, name = as.character(substitute(x))) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(TRUE)
    }
    if (is.character(x) && length(x) == 1 && x %in% choice) {
        return(TRUE)
    }
    msg <- sprintf(
        "`%s` must be one of: %s",
        name,
        paste(choice, collapse = ", ")
    )
    stop(msg, call. = FALSE)
}

check_string <- function(x, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(invisible(TRUE))
    }
    if (is.character(x) && length(x) == 1) {
        return(invisible(TRUE))
    }
    return(FALSE)
}

assert_string <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    msg <- sprintf("`%s` must be a string.", name)
    if (!isTRUE(check_string(x, null.ok = null.ok))) {
        stop(msg, call. = FALSE)
    }
}

check_flag <- function(x, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(TRUE)
    }
    if (is.logical(x) && length(x) == 1) {
        return(TRUE)
    }
    return(FALSE)
}

assert_flag <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    msg <- sprintf("`%s` must be a logical flag.", name)
    if (!isTRUE(check_flag(x, null.ok = null.ok))) {
        stop(msg, call. = FALSE)
    }
}

check_function <- function(x, null.ok = FALSE) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(TRUE)
    }
    if (is.function(x)) {
        return(TRUE)
    }
    return(FALSE)
}

assert_function <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    msg <- sprintf("`%s` must be a function.", name)
    if (!isTRUE(check_function(x, null.ok = null.ok))) {
        stop(msg, call. = FALSE)
    }
}

assert_length <- function(x, len = 1, null.ok = FALSE, name = as.character(substitute(x))) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(invisible(TRUE))
    }
    msg <- sprintf("`%s` must be one of these lengths: %s", name, paste(len, collapse = ", "))
    if (!length(x) %in% len) {
        stop(msg, call. = FALSE)
    }
}

assert_logical <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(invisible(TRUE))
    }
    msg <- sprintf("`%s` must be a logical vector", name)
    if (!is.logical(x)) stop(msg, call. = FALSE)
}


check_integerish <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = TRUE) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(TRUE)
    }
    if (!is.numeric(x)) {
        return(FALSE)
    }
    x <- stats::na.omit(x)
    if (!is.null(len) && length(x) != len) {
        return(FALSE)
    }
    if (!is.null(lower) && any(x < lower)) {
        return(FALSE)
    }
    if (!is.null(upper) && any(x > upper)) {
        return(FALSE)
    }
    if (isTRUE(any(abs(x - round(x)) > (.Machine$double.eps)^0.5))) {
        return(FALSE)
    }
    return(TRUE)
}


assert_integerish <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
    if (isTRUE(null.ok) && is.null(x)) {
        return(invisible())
    }
    msg <- sprintf("`%s` must be integer-ish", name)
    if (is.null(x) && !isTRUE(null.ok)) stop(sprintf("%s should not be NULL.", name), call. = FALSE)
    if (!isTRUE(check_integerish(x, len = len, lower = lower, upper = upper, null.ok = null.ok))) {
        if (!is.numeric(x)) msg <- paste0(msg, "; it is not numeric")
        if (!is.null(len) && length(x) != len) msg <- paste0(msg, sprintf("; its length must be %s", len))
        if (!is.null(lower) && any(x < lower)) msg <- paste0(msg, sprintf("; all values must be greater than or equal to %s", lower))
        if (!is.null(upper) && any(x > upper)) msg <- paste0(msg, sprintf("; all values must be less than or equal to %s", upper))
        if (isTRUE(any(abs(x - round(x)) > (.Machine$double.eps)^0.5))) msg <- paste0(msg, "; all values must be close to integers")
        stop(msg, call. = FALSE)
    }
}


check_null <- function(x) {
    is.null(x)
}

assert_null <- function(x, name = as.character(substitute(x))) {
    if (!isTRUE(check_null(x))) stop(sprintf("%s should be NULL.", name), call. = FALSE)
}

check_numeric <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = TRUE) {
    if (is.null(x) && isTRUE(null.ok)) {
        return(TRUE)
    }
    if (!is.numeric(x)) {
        return(FALSE)
    }
    if (!is.null(len) && length(x) != len) {
        return(FALSE)
    }
    if (!is.null(lower) && any(x < lower)) {
        return(FALSE)
    }
    if (!is.null(upper) && any(x > upper)) {
        return(FALSE)
    }
    return(TRUE)
}


assert_numeric <- function(x, len = NULL, lower = NULL, upper = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
    msg <- sprintf("`%s` must be numeric", name)
    if (!isTRUE(check_numeric(x, len = len, lower = lower, upper = upper, null.ok = null.ok))) {
        if (!is.null(len) && length(x) != len) msg <- paste0(msg, sprintf("; its length must be %s", len))
        if (!is.null(lower) && any(x < lower)) msg <- paste0(msg, sprintf("; all values must be greater than or equal to %s", lower))
        if (!is.null(upper) && any(x > upper)) msg <- paste0(msg, sprintf("; all values must be less than or equal to %s", upper))
        stop(msg, call. = FALSE)
    }
}


assert_data_frame <- function(x, min_rows = 0, min_cols = 0, name = as.character(substitute(x))) {
    msg <- sprintf("`%s` must be a data.frame.", name)
    if (!is.data.frame(x)) stop(msg, call. = FALSE)
    msg <- sprintf("Number of rows in `%s` must be at least `%s`", name, min_rows)
    if (nrow(x) < min_rows) stop(msg, call. = FALSE)
    msg <- sprintf("Number of columns in `%s` must be at least `%s`", name, min_cols)
    if (ncol(x) < min_cols) stop(msg, call. = FALSE)
}


check_character <- function(x, len = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
    if (isTRUE(null.ok) && is.null(x)) {
        return(TRUE)
    } else if (!is.character(x)) {
        msg <- sprintf("`%s` must be character.", name)
        return(msg)
    } else if (!is.null(len) && length(x) != len) {
        msg <- sprintf("`%s` must have length %s.", name, len)
        return(msg)
    }
    return(TRUE)
}

assert_character <- function(x, len = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
    flag <- check_character(x, len = len, null.ok = null.ok, name = name)
    if (!isTRUE(flag)) {
        stop(flag, call. = FALSE)
    } else {
        return(invisible(TRUE))
    }
}

assert_list <- function(x, named = FALSE, len = NULL, null.ok = FALSE, name = as.character(substitute(x))) {
    if (isTRUE(null.ok) && is.null(x)) {
        return(invisible(TRUE))
    }
    if (!is.list(x)) stop("Input is not a list.", call. = FALSE)
    if (isTRUE(named)) {
        if (is.null(names(x))) {
            stop(sprintf("`%s` should be named list.", name), call. = FALSE)
        }
    }
    if (!is.null(len)) {
        if (length(x) != len) {
            stop(sprintf("`%s` must be of length %s.", name, len), call. = FALSE)
        }
    }
}


assert_function <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    if (isTRUE(null.ok) && is.null(x)) {
        return(invisible(TRUE))
    }
    if (!is.function(x)) {
        msg <- sprintf("`%s` must be a function.", name)
        stop(msg, call. = FALSE)
    }
}

check_atomic_vector <- function(x, null.ok = FALSE, name = as.character(substitute(x))) {
    if (isTRUE(null.ok) && is.null(x)) {
        return(invisible(TRUE))
    }
    # doesn't work on glue::glue() output
    # flag <- is.atomic(x) && is.vector(x) && !is.list(x)
    flag <- is.atomic(x) && is.null(dim(x)) && length(x) > 0 && !is.list(x)
    if (flag) {
        out <- TRUE
    } else if (is.factor(x) && is.null(dim(x))) {
        out <- TRUE
    } else {
        out <- sprintf("`%s` must be an atomic vector.", name)
    }
    return(out)
}


assert_class <- function(x, classname) {
    if (!inherits(x, classname)) {
        msg <- sprintf("`x` must be of class `%s`.", classname)
        stop(msg, call. = FALSE)
    }
}


sanitize_notes <- function(notes) {
    if (is.character(notes) && length(notes) > 0) {
        notes <- as.list(notes)
    }
    assert_list(notes, null.ok = TRUE)
    for (idx in seq_along(notes)) {
        n <- notes[[idx]]
        bad <- FALSE
        if (is.list(n)) {
            if (is.null(names(notes)[idx])) {
                bad <- TRUE
            }
            if (!all(c("i", "j", "text") %in% names(n))) {
                bad <- TRUE
            }
        } else if (!is.character(n) || length(n) != 1) {
            bad <- TRUE
        }
        if (isTRUE(bad)) {
            stop("`notes` includes invalid elements. Please refer to the documentation for details.", call. = FALSE)
        }
    }
    return(notes)
}


sanitize_replace <- function(replace) {
    if (isTRUE(replace)) {
        replace <- stats::setNames(list(NA), "")
    } else if (isFALSE(replace)) {
        replace <- stats::setNames(list(NULL), "")
    } else if (isTRUE(check_string(replace))) {
        replace <- stats::setNames(list(NA), replace)
    } else if (!is.list(replace) || is.null(names(replace))) {
        stop("`replace` should be TRUE/FALSE, a single string, or a named list.", call. = FALSE)
    }
    return(replace)
}


sanity_num_mark <- function(digits, num_mark_big, num_mark_dec) {
    if (is.null(digits)) {
        if (num_mark_big != "") stop("`num_mark_big` requires a `digits` value.", call. = FALSE)
        if (num_mark_dec != ".") stop("`num_mark_dec` requires a `digits` value.", call. = FALSE)
    }
}
