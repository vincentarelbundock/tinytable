# =============================================================================
# SHARED TABULARRAY HELPER FUNCTIONS
# =============================================================================

#' Insert content into tabularray string at specific locations
#' @keywords internal
#' @noRd
insert_tabularray_content <- function(x, content = NULL, type = "body") {
    out <- x

    out <- strsplit(out, "\n")[[1]]
    comment <- switch(type,
        "body" = "% tabularray inner close",
        "outer" = "% tabularray outer close",
        "inner" = "% tabularray inner close"
    )
    idx <- grep(comment, out)

    if (length(content) > 0) {
        content <- trimws(content)
        if (!grepl(",$", content) && type != "body") {
            content <- paste0(content, ",")
        }
        if (type == "body") {
            out <- c(out[1:idx], content, out[(idx + 1):length(out)])
        } else {
            out <- c(out[1:(idx - 1)], content, out[idx:length(out)])
        }
    }

    out <- paste(out, collapse = "\n")

    return(out)
}

#' Define color in tabularray preamble
#' @keywords internal
#' @noRd
define_color_preamble <- function(x, col) {
    if (grepl("^#", col)) {
        # hex color need to be defined in LaTeX
        col <- sub("^#", "c", col)
        regex <- sprintf("DefineColor.*%s", col)
        if (!grepl(regex, x@table_string)) {
            b <- sprintf(
                "\\tinytableDefineColor{%s}{HTML}{%s}",
                col,
                sub("^c", "", col)
            )
            x@table_string <- insert_tabularray_content(
                x@table_string,
                content = b,
                type = "body"
            )
        }
    }
    return(x)
}

#' Calculate d-column specification for tabularray
#' @keywords internal
#' @noRd
calculate_dcolumn_spec <- function(j, x) {
    siunitx <- get_option(
        "tinytable_siunitx_table_format",
        default = "table-format=-%s.%s,table-align-text-before=false,table-align-text-after=false,input-symbols={-,\\*+()}"
    )
    num <- unlist(x@data_body[, j])

    # empty cells
    num <- sapply(num, trimws)
    num <- num[sapply(num, nchar) > 0]

    num <- strsplit(num, "\\.")
    num <- lapply(num, function(k) if (length(k) == 1) c(k, " ") else k)

    left <- sapply(num, function(k) k[[1]])
    right <- sapply(num, function(k) k[[2]])
    left <- max(nchar(gsub("\\D", "", left)))
    right <- max(nchar(gsub("\\D", "", right)))
    out <- sprintf(siunitx, left, right)
    out <- sprintf("si={%s},", out)
    return(out)
}

#' Generate LaTeX range string for tabularray
#' @keywords internal
#' @noRd
latex_range_string <- function(x) {
    if (length(x) == 0) {
        return("")
    }
    x <- sort(unique(x))
    start <- x[c(TRUE, diff(x) != 1)]
    end <- x[c(diff(x) != 1, TRUE)]
    parts <- ifelse(start == end, start, paste0(start, "-", end))
    paste(parts, collapse = ",")
}

#' Find consecutive spans in group data
#' @keywords internal
#' @noRd
find_consecutive_spans <- function(group_row) {
    spans <- list()
    i <- 1

    while (i <= length(group_row)) {
        current_label <- group_row[i]
        span_start <- i

        # Skip NA (ungrouped) columns
        if (is.na(current_label)) {
            i <- i + 1
            next
        }

        # Find the end of this span
        # Only continue while we see empty strings - stop at any non-empty label (even if same)
        if (trimws(current_label) != "") {
            i <- i + 1 # Move past the current label
            # Continue only through empty strings
            while (
                i <= length(group_row) &&
                    !is.na(group_row[i]) &&
                    trimws(group_row[i]) == ""
            ) {
                i <- i + 1
            }
        } else {
            while (
                i <= length(group_row) &&
                    !is.na(group_row[i]) &&
                    trimws(group_row[i]) == ""
            ) {
                i <- i + 1
            }
        }
        span_end <- i - 1

        # Only add non-empty labels
        if (trimws(current_label) != "") {
            spans[[length(spans) + 1]] <- list(
                label = current_label,
                start = span_start,
                end = span_end,
                length = span_end - span_start + 1
            )
        }
    }

    return(spans)
}

#' Build tabularray header row from group data
#' @keywords internal
#' @noRd
build_tabularray_header <- function(group_row, ncols) {
    header <- rep("", ncols)
    cmidrules <- character(0)

    spans <- find_consecutive_spans(group_row)

    for (span in spans) {
        header[span$start] <- span$label
        cmidrules <- NULL
        # cmidrules <- c(
        #     cmidrules,
        #     sprintf("\\cmidrule[lr]{%s-%s}", span$start, span$end)
        # )
    }

    header_line <- paste(header, collapse = " & ")
    header_line <- paste(header_line, "\\\\", paste(cmidrules, collapse = ""))

    return(header_line)
}

#' Insert header row into tabularray string
#' @keywords internal
#' @noRd
insert_tabularray_header <- function(x, header_line) {
    out <- strsplit(x@table_string, split = "\\n")[[1]]

    # Insert the header line
    idx <- max(
        c(
            grep("% tabularray inner close", out),
            grep("\\toprule", out, fixed = TRUE)
        )
    )

    out <- c(
        out[1:idx],
        trimws(header_line),
        out[(idx + 1):length(out)]
    )

    out <- paste(out, collapse = "\n")
    x@table_string <- out

    return(x)
}

#' Apply styling to tabularray header spans
#' @keywords internal
#' @noRd
style_tabularray_header_spans <- function(x, group_row, row_ihead) {
    spans <- find_consecutive_spans(group_row)

    for (span in spans) {
        cs <- if (span$length == 1) NULL else span$length
        args <- list(
            tt_build_now = TRUE,
            x = x,
            i = row_ihead,
            j = span$start,
            align = "c",
            colspan = cs
        )
        x <- do.call(style_tt, args)
    }

    return(x)
}
