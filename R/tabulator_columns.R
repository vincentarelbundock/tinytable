# -----------------------------------------------------------------------------
# constants
# -----------------------------------------------------------------------------
TAB_NUM <- c("integer", "numeric", "double")
TAB_DATE <- c("Date", "POSIXct", "POSIXlt")
TAB_FORM <- c(TAB_NUM, "logical", TAB_DATE)

# small helpers
merge_lists <- function(x, y) { # shallow merge; rhs wins
    x[names(y)] <- y
    x
}

# Translate an R strptime format string to Luxon tokens for Tabulator's
# datetime formatter (outputFormat). Known %-tokens are mapped; literal ASCII
# letters are wrapped in single quotes so Luxon does not interpret them as
# tokens; other characters (separators, spaces, ...) pass through as-is.
# Strings without any % token (e.g. an already-Luxon "M/d/yyyy") are returned
# unchanged.
strptime_to_luxon <- function(fmt) {
    if (!is.character(fmt) || length(fmt) != 1 || !grepl("%", fmt, fixed = TRUE)) {
        return(fmt)
    }
    map <- c(
        "Y" = "yyyy", "y" = "yy", "m" = "MM", "d" = "dd", "e" = "d",
        "H" = "HH", "I" = "hh", "M" = "mm", "S" = "ss", "p" = "a",
        "B" = "MMMM", "b" = "MMM", "A" = "cccc", "a" = "ccc", "j" = "ooo"
    )
    chars <- strsplit(fmt, "", fixed = TRUE)[[1]]
    out <- character(0)
    i <- 1
    n <- length(chars)
    while (i <= n) {
        if (chars[i] == "%" && i < n) {
            token <- chars[i + 1]
            if (token == "%") {
                out <- c(out, "%")
            } else if (token %in% names(map)) {
                out <- c(out, map[[token]])
            } else {
                # unknown token: pass through untranslated
                out <- c(out, "%", token)
            }
            i <- i + 2
        } else if (grepl("[A-Za-z]", chars[i])) {
            # quote runs of literal letters so Luxon treats them as text
            run <- i
            while (run < n && grepl("[A-Za-z]", chars[run + 1])) {
                run <- run + 1
            }
            out <- c(out, "'", chars[i:run], "'")
            i <- run + 1
        } else {
            out <- c(out, chars[i])
            i <- i + 1
        }
    }
    paste(out, collapse = "")
}

# -----------------------------------------------------------------------------
# single formatter registry (data-driven)
# Each entry returns a list(title/field/formatter/params/...),
# given (col_def, args).
# -----------------------------------------------------------------------------
tabulator_tickcross_params <- function() {
    list(
        allowEmpty = TRUE,
        allowTruthy = TRUE,
        tickElement = "<i class='fa-solid fa-check'></i>",
        crossElement = "<i class='fa-solid fa-xmark'></i>"
    )
}

tabulator_column_registry <- list(
    numeric = function(col_def, args) {
        digits <- args$digits %||% get_option("tinytable_format_digits")
        thousand <- args$num_mark_big %||% get_option("tinytable_format_num_mark_big", "")
        decimal <- args$num_mark_dec %||% get_option("tinytable_format_num_mark_dec", get_option("OutDec", "."))
        # Note: Tabulator's `money` formatter only supports a fixed number of
        # decimal places (`precision`), so `num_fmt` ("significant",
        # "scientific", ...) and `num_zero` are not honored client-side.

        merge_lists(col_def, list(
            formatter = "money",
            formatterParams = list(
                decimal = decimal,
                thousand = thousand,
                precision = digits %||% 2,
                symbol = "",
                symbolAfter = FALSE
            )
        ))
    },
    logical = function(col_def, args) {
        bool_fun <- args$bool %||% get_option("tinytable_format_bool")
        if (!is.null(bool_fun) && is.function(bool_fun)) {
            merge_lists(col_def, list(
                formatter = "plaintext"
            ))
        } else {
            merge_lists(col_def, list(
                formatter = "tickCross",
                formatterParams = tabulator_tickcross_params()
            ))
        }
    },
    Date = function(col_def, args) {
        fmt_out <- strptime_to_luxon(args$date %||% get_option("tinytable_format_date") %||% "M/d/yyyy")
        merge_lists(col_def, list(
            formatter = "datetime",
            sorter = "datetime",
            formatterParams = list(
                inputFormat = "yyyy-MM-dd",
                outputFormat = fmt_out,
                invalidPlaceholder = ""
            ),
            sorterParams = list(
                format = "yyyy-MM-dd",
                alignEmptyValues = "bottom"
            )
        ))
    },
    POSIXct = function(col_def, args) {
        fmt_out <- strptime_to_luxon(args$date %||% get_option("tinytable_format_date") %||% "M/d/yyyy HH:mm:ss")
        merge_lists(col_def, list(
            formatter = "datetime",
            sorter = "datetime",
            formatterParams = list(
                inputFormat = "yyyy-MM-dd HH:mm:ss",
                outputFormat = fmt_out,
                invalidPlaceholder = ""
            ),
            sorterParams = list(
                format = "yyyy-MM-dd HH:mm:ss",
                alignEmptyValues = "bottom"
            )
        ))
    },
    POSIXlt = function(col_def, args) {
        tabulator_column_registry$POSIXct(col_def, args)
    })

# -----------------------------------------------------------------------------
# inject the columns JSON into the table string exactly once
# - replaces the template placeholder when still present, otherwise rewrites the
#   previously injected `columns: [...]` line
# - the replacement is inserted literally (gsub replacement semantics would
#   otherwise mangle backslashes in properly escaped JSON)
# -----------------------------------------------------------------------------
tabulator_replace_columns_json <- function(table_string, columns_json) {
    if (grepl("$tinytable_TABULATOR_COLUMNS", table_string, fixed = TRUE)) {
        return(gsub(
            "$tinytable_TABULATOR_COLUMNS",
            columns_json,
            table_string,
            fixed = TRUE
        ))
    }
    # columns JSON is emitted on a single line: match to the last `]` on it
    literal <- gsub("\\", "\\\\", columns_json, fixed = TRUE)
    sub(
        "columns: \\[[^\n]*\\]",
        paste0("columns: ", literal),
        table_string
    )
}

# -----------------------------------------------------------------------------
# one function to build the columns, applying
# 1) base spec, 2) lazy-format overrides, 3) style overrides, 4) write JSON
# -----------------------------------------------------------------------------
tabulator_apply_columns <- function(x) {
    stopifnot(!is.null(x@names), length(x@names) > 0)

    # 1) base column specs
    # Clean the full vector at once so de-duplication is consistent
    fields <- tabulator_clean_column_name(x@names)
    columns <- lapply(seq_along(x@data), function(j) {
        col_name <- x@names[j]
        field <- fields[j]
        ctype <- class(x@data[[j]])[1]

        col_def <- list(title = col_name, field = field)

        if (ctype %in% TAB_FORM) {
            # pick the registry key
            key <- if (ctype %in% TAB_NUM) "numeric" else ctype
            formatter_fun <- tabulator_column_registry[[key]]
            if (!is.null(formatter_fun)) {
                col_def <- formatter_fun(col_def, args = list())
            }
        }

        col_def
    })
    names(columns) <- vapply(columns, `[[`, character(1), "title")

    # 2) lazy-format to per-column overrides (digits/date/marks/etc.)
    if (length(x@lazy_format) > 0) {
        # build a map: title -> merged formatter col_def
        for (l in x@lazy_format) {
            js <- if (is.null(l$j)) seq_along(x@data) else sanitize_j(l$j, x)
            for (j in js) {
                col_name <- x@names[j]
                ctype <- class(x@data[[j]])[1]
                if (!(ctype %in% TAB_FORM)) next

                key <- if (ctype %in% TAB_NUM) "numeric" else ctype
                f <- tabulator_column_registry[[key]]
                if (is.null(f)) next

                # args from lazy_format
                args <- list(
                    digits       = l$digits,
                    num_fmt      = l$num_fmt,
                    num_mark_big = l$num_mark_big,
                    num_mark_dec = l$num_mark_dec,
                    num_zero     = l$num_zero,
                    date         = l$date_format,
                    bool         = l$bool
                )
                columns[[col_name]] <- f(columns[[col_name]], args)
            }
        }
    }

    # 3) column-level styles (hozAlign, vertAlign, ...)
    if (length(x@tabulator_column_styles) > 0) {
        for (nm in names(x@tabulator_column_styles)) {
            if (!nm %in% names(columns)) next
            st <- x@tabulator_column_styles[[nm]]
            if (!is.null(st$hozAlign)) columns[[nm]]$hozAlign <- st$hozAlign
            if (!is.null(st$vertAlign)) columns[[nm]]$vertAlign <- st$vertAlign
        }
    }

    # 3.5) apply column formatters from plot_tt and other sources
    if (length(x@tabulator_column_formatters) > 0) {
        for (nm in names(x@tabulator_column_formatters)) {
            if (!nm %in% names(columns)) next
            fmt <- x@tabulator_column_formatters[[nm]]
            if (!is.null(fmt$formatter)) columns[[nm]]$formatter <- fmt$formatter
            if (!is.null(fmt$formatterParams)) columns[[nm]]$formatterParams <- fmt$formatterParams
            if (!is.null(fmt$sorter)) columns[[nm]]$sorter <- fmt$sorter
            if (!is.null(fmt$sorterParams)) columns[[nm]]$sorterParams <- fmt$sorterParams
        }
    }

    # 4) persist back and inject JSON
    x@tabulator_columns <- unname(columns)

    columns_json <- df_to_json(x@tabulator_columns)
    x@table_string <- tabulator_replace_columns_json(x@table_string, columns_json)

    x
}
