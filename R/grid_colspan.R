grid_colspan <- function(x) {
    if (!x@output %in% c("markdown", "dataframe")) {
        return(x)
    }

    sty <- prepare_grid_style(x)

    if (nrow(sty) == 0 || !is.data.frame(sty)) {
        return(x)
    }

    # Check if colspan column exists before accessing it
    if (!"colspan" %in% colnames(sty)) {
        return(x)
    }

    # Find rows with colspan > 1
    colspan_rows <- sty[!is.na(sty$colspan) & sty$colspan > 1, ]

    if (nrow(colspan_rows) == 0) {
        return(x)
    }

    # Get the table string and split into lines
    table_lines <- strsplit(x@table_string, "\\n")[[1]]

    # Find the header separator line (+=====+) as reference point
    header_sep_line <- which(grepl("^\\+={2,}", table_lines))
    if (length(header_sep_line) == 0) {
        # No header separator found, try to find any separator line starting with +
        header_sep_line <- which(grepl("^\\+-", table_lines))[1]
        if (is.na(header_sep_line)) {
            return(x) # Can't find reference point
        }
    } else {
        header_sep_line <- header_sep_line[1]
    }

    for (idx in seq_len(nrow(colspan_rows))) {
        row_idx <- colspan_rows[idx, "i"]
        col_idx <- colspan_rows[idx, "j"]
        colspan <- colspan_rows[idx, "colspan"]

        # Calculate target line: header_sep_line + row_idx (rows are consecutive after header separator)
        target_line <- header_sep_line + row_idx

        if (
            target_line <= length(table_lines) &&
                startsWith(table_lines[target_line], "|")
        ) {
            line <- table_lines[target_line]

            # Split the line by | to get cells
            cells <- strsplit(line, "\\|")[[1]]

            # Remove empty first and last elements
            if (length(cells) > 0 && cells[1] == "") {
                cells <- cells[-1]
            }
            if (length(cells) > 0 && cells[length(cells)] == "") {
                cells <- cells[-length(cells)]
            }

            # Remove | markers for the colspan range
            if (
                col_idx <= length(cells) &&
                    (col_idx + colspan - 1) <= length(cells)
            ) {
                # For colspan, use only the content from the first cell (the spanning cell)
                # and ignore content from subsequent cells that are being spanned over
                # Preserve leading space but trim trailing spaces
                spanned_content <- sub(" *$", "", cells[col_idx])

                # Calculate total width for the spanned cells
                if (
                    !is.null(x@width_cols) &&
                        length(x@width_cols) >= (col_idx + colspan - 1)
                ) {
                    total_width <- sum(x@width_cols[
                        col_idx:(col_idx + colspan - 1)
                    ]) +
                        (colspan - 1)
                    current_width <- ansi_nchar(spanned_content)
                    if (current_width < total_width) {
                        padding <- total_width - current_width
                        spanned_content <- paste0(
                            spanned_content,
                            strrep(" ", padding)
                        )
                    }
                }

                # Replace the cells with the spanned content
                new_cells <- cells
                new_cells[col_idx] <- spanned_content
                # Remove the subsequent cells that are part of the colspan
                if (colspan > 1) {
                    indices_to_remove <- (col_idx + 1):(col_idx + colspan - 1)
                    new_cells <- new_cells[-indices_to_remove]
                }

                # Reconstruct the line, preserving the original format
                table_lines[target_line] <- paste0(
                    "|",
                    paste(new_cells, collapse = "|"),
                    "|"
                )
            }
        }
    }

    # Reconstruct the table string
    x@table_string <- paste(table_lines, collapse = "\n")

    return(x)
}
