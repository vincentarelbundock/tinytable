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
        if (type == "body") {
            # Preserve the sequential single-insert semantics: each element is
            # placed immediately after the marker, so a vector lands reversed.
            out <- c(out[1:idx], rev(content), out[(idx + 1):length(out)])
        } else {
            content <- paste0(content, ifelse(grepl(",$", content), "", ","))
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
        # Match the definition token itself, not the color name anywhere in the
        # document (cell text containing the token must not suppress it).
        regex <- sprintf("DefineColor\\{%s\\}", col)
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

#' Build tabularray header row from group data
#' @keywords internal
#' @noRd
build_tabularray_header <- function(group_row, ncols) {
    header <- rep("", ncols)

    spans <- parse_group_spans(group_row)

    for (span_idx in seq_len(nrow(spans))) {
        header[spans$start[span_idx]] <- spans$label[span_idx]
    }

    header_line <- paste(header, collapse = " & ")
    header_line <- paste(header_line, "\\\\", "")

    return(header_line)
}

#' Insert header row into tabularray string
#' @keywords internal
#' @noRd
insert_tabularray_header <- function(x, header_line) {
    out <- strsplit(x@table_string, split = "\\n")[[1]]

    # Insert the header line
    idx_candidates <- c(
        grep("% tabularray inner close", out),
        grep("\\toprule", out, fixed = TRUE)
    )
    if (length(idx_candidates) == 0) {
        stop(
            "tinytable: cannot locate insertion point for column group header.",
            call. = FALSE
        )
    }
    idx <- max(idx_candidates)

    out <- c(
        out[1:idx],
        trimws(header_line),
        out[(idx + 1):length(out)]
    )

    out <- paste(out, collapse = "\n")
    x@table_string <- out

    return(x)
}

