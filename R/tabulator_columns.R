# -----------------------------------------------------------------------------
# constants
# -----------------------------------------------------------------------------
TAB_NUM <- c("integer", "numeric", "double")
TAB_DATE <- c("Date", "POSIXct", "POSIXlt")
TAB_FORM <- c(TAB_NUM, "logical", TAB_DATE)

# small helpers
`%||%` <- function(a, b) if (is.null(a)) b else a
col_type1 <- function(x) class(x)[1]
merge_lists <- function(x, y) { # shallow merge; rhs wins
    x[names(y)] <- y
    x
}

# -----------------------------------------------------------------------------
# single formatter registry (data-driven)
# Each entry returns a list(title/field/formatter/params/…),
# given (col_def, x, j, args).
# -----------------------------------------------------------------------------
tabulator_column_registry <- list(
    numeric = function(col_def, x, j, args) {
        digits <- args$digits %||% get_option("tinytable_format_digits")
        num_fmt <- args$num_fmt %||% get_option("tinytable_format_num_fmt", "significant")
        thousand <- args$num_mark_big %||% get_option("tinytable_format_num_mark_big", "")
        decimal <- args$num_mark_dec %||% get_option("tinytable_format_num_mark_dec", get_option("OutDec", "."))
        # num_zero   <- args$num_zero    %||% get_option("tinytable_format_num_zero", FALSE)  # keep if needed

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
    logical = function(col_def, x, j, args) {
        bool_fun <- args$bool %||% get_option("tinytable_format_bool")
        merge_lists(col_def, list(
            formatter = if (!is.null(bool_fun) && is.function(bool_fun)) "plaintext" else "tickCross"
        ))
    },
    Date = function(col_def, x, j, args) {
        fmt_out <- args$date %||% get_option("tinytable_format_date") %||% "M/d/yyyy"
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
    POSIXct = function(col_def, x, j, args) {
        fmt_out <- args$date %||% get_option("tinytable_format_date") %||% "M/d/yyyy HH:mm:ss"
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
    POSIXlt = function(col_def, x, j, args) {
        tabulator_column_registry$POSIXct(col_def, x, j, args)
    })

# -----------------------------------------------------------------------------
# one function to build the columns, applying
# 1) base spec, 2) lazy-format overrides, 3) style overrides, 4) write JSON
# -----------------------------------------------------------------------------
tabulator_apply_columns <- function(x) {
    stopifnot(!is.null(x@names), length(x@names) > 0)

    # 1) base column specs
    columns <- lapply(seq_along(x@data), function(j) {
        col_name <- x@names[j]
        field <- tabulator_clean_column_name(col_name)
        ctype <- col_type1(x@data[[j]])

        col_def <- list(title = col_name, field = field)

        if (ctype %in% TAB_FORM) {
            # pick the registry key
            key <- if (ctype %in% TAB_NUM) "numeric" else ctype
            formatter_fun <- tabulator_column_registry[[key]]
            if (!is.null(formatter_fun)) {
                col_def <- formatter_fun(col_def, x, j, args = list())
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
                ctype <- col_type1(x@data[[j]])
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
                columns[[col_name]] <- f(columns[[col_name]], x, j, args)
            }
        }
    }

    # 3) column-level styles (hozAlign, vertAlign, …)
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
    x@table_string <- gsub("$tinytable_TABULATOR_COLUMNS", columns_json, x@table_string, fixed = TRUE)
    x@table_string <- gsub("columns: \\[.*?\\]", paste0("columns: ", columns_json), x@table_string)

    x
}
