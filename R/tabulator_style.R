#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_tabulator",
  definition = function(x,
                        i = NULL,
                        j = NULL,
                        bold = FALSE,
                        italic = FALSE,
                        monospace = FALSE,
                        underline = FALSE,
                        strikeout = FALSE,
                        color = NULL,
                        background = NULL,
                        fontsize = NULL,
                        align = NULL,
                        alignv = NULL,
                        line = NULL,
                        line_color = "black",
                        line_width = 0.1,
                        colspan = NULL,
                        rowspan = NULL,
                        indent = 0,
                        ...) {
    # Check that row-based styling (i argument) is not supported
    if (!is.null(i)) {
      stop(
        "Row-based styling (i argument) is not supported with tabulator output. ",
        "Use column-based styling (j argument) instead.",
        call. = FALSE
      )
    }

    # Process alignment styles from the @style data frame
    if (nrow(x@style) > 0) {
      for (row_idx in seq_len(nrow(x@style))) {
        style_row <- x@style[row_idx, ]

        # Skip rows with no alignment settings
        if (is.na(style_row$align) && is.na(style_row$alignv)) {
          next
        }

        # Get column indices for this style row
        if (is.na(style_row$i) && !is.na(style_row$j)) {
          # Column-based styling
          j_clean <- style_row$j
        } else {
          # Skip row-based styling - not supported
          next
        }

        # Apply styling to the column
        col_name <- x@names[j_clean]

        # Initialize column styling if it doesn't exist
        if (is.null(x@tabulator_column_styles)) {
          x@tabulator_column_styles <- list()
        }
        if (is.null(x@tabulator_column_styles[[col_name]])) {
          x@tabulator_column_styles[[col_name]] <- list()
        }

        # Map tinytable align values to Tabulator hozAlign
        if (!is.na(style_row$align)) {
          tabulator_align <- switch(style_row$align,
            "l" = "left",
            "c" = "center",
            "r" = "right",
            style_row$align # pass through if already in Tabulator format
          )
          x@tabulator_column_styles[[col_name]]$hozAlign <- tabulator_align
        }

        # Map tinytable alignv values to Tabulator vertAlign
        if (!is.na(style_row$alignv)) {
          tabulator_alignv <- switch(style_row$alignv,
            "t" = "top",
            "m" = "middle",
            "b" = "bottom",
            style_row$alignv # pass through if already in Tabulator format
          )
          x@tabulator_column_styles[[col_name]]$vertAlign <- tabulator_alignv
        }
      }
    }

    # Warn about unsupported styling properties found in @style data frame
    unsupported_styles <- c()
    if (nrow(x@style) > 0) {
      for (row_idx in seq_len(nrow(x@style))) {
        style_row <- x@style[row_idx, ]

        if (isTRUE(style_row$bold)) unsupported_styles <- c(unsupported_styles, "bold")
        if (isTRUE(style_row$italic)) unsupported_styles <- c(unsupported_styles, "italic")
        if (isTRUE(style_row$monospace)) unsupported_styles <- c(unsupported_styles, "monospace")
        if (isTRUE(style_row$underline)) unsupported_styles <- c(unsupported_styles, "underline")
        if (isTRUE(style_row$strikeout)) unsupported_styles <- c(unsupported_styles, "strikeout")
        if ("color" %in% names(style_row) && !is.na(style_row$color)) unsupported_styles <- c(unsupported_styles, "color")
        if ("background" %in% names(style_row) && !is.na(style_row$background)) unsupported_styles <- c(unsupported_styles, "background")
        if ("fontsize" %in% names(style_row) && !is.na(style_row$fontsize)) unsupported_styles <- c(unsupported_styles, "fontsize")
        if ("line" %in% names(style_row) && !is.na(style_row$line)) unsupported_styles <- c(unsupported_styles, "line")
        if ("colspan" %in% names(style_row) && !is.na(style_row$colspan)) unsupported_styles <- c(unsupported_styles, "colspan")
        if ("rowspan" %in% names(style_row) && !is.na(style_row$rowspan)) unsupported_styles <- c(unsupported_styles, "rowspan")
        if ("indent" %in% names(style_row) && !is.na(style_row$indent)) unsupported_styles <- c(unsupported_styles, "indent")
        if ("bootstrap_class" %in% names(style_row) && !is.na(style_row$bootstrap_class)) unsupported_styles <- c(unsupported_styles, "bootstrap_class")
        if ("bootstrap_css" %in% names(style_row) && !is.na(style_row$bootstrap_css)) unsupported_styles <- c(unsupported_styles, "bootstrap_css")
      }

      unsupported_styles <- unique(unsupported_styles)
      if (length(unsupported_styles) > 0) {
        warning(
          "The following style_tt() arguments are not yet supported with tabulator output: ",
          paste(unsupported_styles, collapse = ", "),
          ". Only 'align' and 'alignv' are currently supported.",
          call. = FALSE
        )
      }
    }

    return(x)
  })
