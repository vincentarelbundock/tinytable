#p' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
setMethod(
  f = "style_eval",
  signature = "tinytable_bootstrap",
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
                        line_width = .1,
                        colspan = NULL,
                        rowspan = NULL,
                        indent = 0,
                        bootstrap_class = NULL,
                        bootstrap_css = NULL,
                        bootstrap_css_rule = NULL,
                        ...) {
    out <- x@table_string

    ival <- sanitize_i(i, x)
    jval <- sanitize_j(j, x)

    # order may be important for recycling
    settings <- expand.grid(i = ival, j = jval, tabularray = "")
    if (is.null(i) && !is.null(j)) {
      settings <- settings[order(settings$i, settings$j), ]
    }

    # JS 0-indexing
    settings$j <- settings$j - 1
    settings$i <- settings$i - 1 + x@nhead


    # settings have a different size for latex, so bootstrap breaks
    vectorize_bootstrap <- function(setting, userinput, string) {
      # simple cases
      if (is.null(userinput) || isFALSE(userinput)) {
        return(setting)
      }
      if (isTRUE(userinput)) {
        return(paste(setting, string))
      }

      # logical vector
      if (is.logical(userinput)) {
        out <- paste(setting, ifelse(userinput, string, ""))
        return(out)
      }

      # character vector means the user inputs actual values
      if (is.character(userinput)) {
        out <- sprintf(string, userinput)
        out <- paste(setting, out)
        return(out)
      }
      stop("here be dragons")
    }

    if (!is.null(align)) {
      align_bootstrap <- ifelse(align == "c", "center", align)
      align_bootstrap <- ifelse(align == "d", "center", align_bootstrap)
      align_bootstrap <- ifelse(align == "l", "left", align_bootstrap)
      align_bootstrap <- ifelse(align == "r", "right", align_bootstrap)
    } else {
      align_bootstrap <- align
    }

    if (!is.null(alignv)) {
      alignv_bootstrap <- switch(alignv,
        "t" = "top",
        "m" = "middle",
        "b" = "bottom"
      )
    } else {
      alignv_bootstrap <- alignv
    }

    if (!is.null(fontsize)) {
      fontsize_bootstrap <- sprintf("%sem", fontsize)
    } else {
      fontsize_bootstrap <- fontsize
    }

    settings$bootstrap <- ""
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, bold, "font-weight: bold;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, italic, "font-style: italic;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, underline, "text-decoration: underline;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, strikeout, "text-decoration: line-through;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, monospace, "font-family: monospace;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, fontsize_bootstrap, "font-size: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, align_bootstrap, "text-align: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, alignv_bootstrap, "vertical-align: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, color, "color: %s;")
    settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, background, "background-color: %s;")
    if (indent > 0) {
      settings$bootstrap <- paste(settings$bootstrap, sprintf("padding-left: %sem;", indent), sep = "")
    }

    if (!is.null(line)) {
      tmp <- sprintf(": solid %s %s;", paste0(line_width, "em"), line_color)
      if (grepl("t", line)) settings$bootstrap <- paste0(settings$bootstrap, " border-top", tmp)
      if (grepl("b", line)) settings$bootstrap <- paste0(settings$bootstrap, " border-bottom", tmp)
      if (grepl("l", line)) settings$bootstrap <- paste0(settings$bootstrap, " border-left", tmp)
      if (grepl("r", line)) settings$bootstrap <- paste0(settings$bootstrap, " border-right", tmp)
    }

    # unique IDs for each CSS style combination
    id <- sapply(unique(settings$bootstrap), function(k) get_id(stem = "tinytable_css_"))
    settings$id <- id[match(settings$bootstrap, names(id))]

    if (is.null(rowspan)) rowspan <- 1
    if (is.null(colspan)) colspan <- 1

    # CSS style for cell
    css_done <- NULL


    # CSS stylers
    for (row in seq_len(nrow(settings))) {
      if (settings$bootstrap[row] != "" || !is.null(bootstrap_css)) {
        # CSS styling
        css <- paste(bootstrap_css, settings$bootstrap[row], collapse = ";")
        css_start <- sprintf(".table td.%s, .table th.%s { ", settings$id[row], settings$id[row])
        css_complete <- paste(c(css_start, css, "}"), collapse = " ")
        # hack: avoid css duplication
        if (!css_complete %in% css_done) {
          out <- bootstrap_setting(out, css_complete, component = "css")
          css_done <- c(css_done, css_complete)
        }
      }
    }

    # CSS listeners
    listener_template <- "
     window.addEventListener('load', function () {
         const cellStyles = [
             %s
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_%s('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });"

    settings_blocks <- split(settings, settings$id)
    for (block in settings_blocks) {
        if (any(block$bootstrap != "")) {
            coords <- expand.grid(block$i, block$j)
            coords <- apply(coords, 1, function(x) sprintf("[%s, %s]", x[1], x[2]))
            coords <- unique(coords)
            coords <- sprintf("{coords: [%s], class: '%s'},", paste(coords, collapse = ", "), settings$id[1])
            listener <- sprintf(listener_template, coords, block$id[1])
            out <- bootstrap_setting(out, listener, component = "cell")
        }
    }

    if (!is.null(bootstrap_css_rule)) {
      out <- bootstrap_setting(out, bootstrap_css_rule, component = "css")
    }

    x@table_string <- out

    if (!is.null(bootstrap_class)) {
      x@bootstrap_class <- bootstrap_class
    }

    return(x)
  })
