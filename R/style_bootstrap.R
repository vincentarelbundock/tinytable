#' Internal styling function
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
                        line_width = 0.1,
                        colspan = NULL,
                        rowspan = NULL,
                        indent = 0,
                        bootstrap_class = NULL,
                        bootstrap_css = NULL,
                        bootstrap_css_rule = NULL,
                        ...) {

    # i is a logical matrix mask
    if (is.matrix(i) && is.logical(i) && nrow(i) == nrow(x) && ncol(i) == ncol(x)) {
      assert_null(j)
      settings <- which(i == TRUE, arr.ind = TRUE)
      settings <- stats::setNames(data.frame(settings), c("i", "j"))
    } else {
      ival <- sanitize_i(i, x)
      jval <- sanitize_j(j, x)
      # order may be important for recycling
      settings <- expand.grid(i = ival, j = jval, tabularray = "")
      if (is.null(i) && !is.null(j)) {
        settings <- settings[order(settings$i, settings$j), ]
      }
    }

    # JS 0-indexing
    settings$j <- settings$j - 1
    settings$i <- settings$i - 1 + x@nhead

    settings[["color"]] <- color
    settings[["color"]] <- if (is.null(color)) NA else color
    settings[["background"]] <- if (is.null(background)) NA else background
    settings[["fontsize"]] <- if (is.null(fontsize)) NA else sprintf("%sem", fontsize)
    settings[["align"]] <- if (is.null(align)) NA else align
    settings[["alignv"]] <- if (is.null(alignv)) NA else alignv
    settings[["line"]] <- if (is.null(line)) NA else line
    settings[["line_color"]] <- line_color
    settings[["line_width"]] <- line_width
    settings[["bold"]] <- bold
    settings[["italic"]] <- italic
    settings[["monospace"]] <- monospace
    settings[["strikeout"]] <- strikeout
    settings[["underline"]] <- underline
    settings[["fontsize"]] <- if (!is.null(fontsize)) sprintf("%sem", fontsize) else NA
    settings[["indent"]] <- indent
    settings[["bootstrap_class"]] <- if (!is.null(bootstrap_class)) bootstrap_class else NA
    settings[["bootstrap_css_rule"]] <- if (!is.null(bootstrap_css_rule)) bootstrap_css_rule else NA
    settings[["bootstrap_css"]] <- if (!is.null(bootstrap_css)) bootstrap_css else NA

    if (nrow(x@style) > 0 && nrow(settings) > 0 && ncol(x@style) != ncol(settings)) {
        a <- x@style
        b <- settings
        if (!"tabularray" %in% colnames(a)) a$tabularray <- ""
        if (!"tabularray" %in% colnames(b)) b$tabularray <- ""
        settings <- rbind(a, b[, colnames(a)])
        x@style <- unique(settings)
    } else if (nrow(settings) > 0) {
        x@style <- rbind(x@style, settings)
    }

    if (!is.null(bootstrap_class)) {
      x@bootstrap_class <- bootstrap_class
    }

    return(x)
  })



style_apply_bootstrap <- function(x) {
    sty <- x@style

    if (any(!is.na(sty$bootstrap_class))) {
        x@bootstrap_class <- last_style_vec(sty$bootstrap_class)
    }

    sty <- last_style(sty)

    sty$line_color[which(sty$line_color == "black")] <- NA

    sty$alignv[which(sty$alignv == "t")] <- "top"
    sty$alignv[which(sty$alignv == "b")] <- "bottom"
    sty$alignv[which(sty$alignv == "m")] <- "middle"

    sty$align[which(sty$align == "l")] <- "left"
    sty$align[which(sty$align == "c")] <- "center"
    sty$align[which(sty$align == "d")] <- "center"
    sty$align[which(sty$align == "r")] <- "right"

    css_arguments <- rep("", nrow(sty))
    idx <- which(sty$bold)
    css_arguments[idx] <- paste(css_arguments[idx], "font-weight: bold;")
    idx <- which(sty$italic)
    css_arguments[idx] <- paste(css_arguments[idx], "font-style: italic;")
    idx <- which(sty$underline)
    css_arguments[idx] <- paste(css_arguments[idx], "text-decoration: underline;")
    idx <- which(sty$strikeout)
    css_arguments[idx] <- paste(css_arguments[idx], "text-decoration: line-through;")
    idx <- which(sty$monospace)
    css_arguments[idx] <- paste(css_arguments[idx], "font-family: monospace;")
    idx <- which(!is.na(sty$color))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("color: ", sty$color[idx], ";"))
    idx <- which(!is.na(sty$background))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("background-color: ", sty$background[idx], ";"))
    idx <- which(!is.na(sty$fontsize))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("font-size: ", sty$fontsize[idx], "em;"))
    idx <- which(!is.na(sty$alignv))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("vertical-align: ", sty$alignv[idx], ";"))
    idx <- which(!is.na(sty$align))
    css_arguments[idx] <- paste(css_arguments[idx], paste0("text-align: ", sty$align[idx], ";"))
    idx <- which(sty$indent != 0)
    css_arguments[idx] <- paste(css_arguments[idx], paste0("padding-left: ", sty$indent[idx], "em;"))

    lincol <- ifelse(is.na(sty$line_color), 
        sprintf("solid %sem", sty$line_width),
        sprintf("solid %s %sem", sty$line_color, sty$line_width))
    lin <- rep("", nrow(sty))
    lin <- ifelse(!grepl("t", sty$line), lin, paste0(lin, sprintf("border-top: %s;", lincol)))
    lin <- ifelse(!grepl("b", sty$line), lin, paste0(lin, sprintf("border-bottom: %s;", lincol)))
    lin <- ifelse(!grepl("l", sty$line), lin, paste0(lin, sprintf("border-left: %s;", lincol)))
    lin <- ifelse(!grepl("r", sty$line), lin, paste0(lin, sprintf("border-right: %s;", lincol)))
    directions <- ifelse(is.na(sty$line), 0, sapply(strsplit(sty$line, ""), function(x) length(unique(x))))
    idx <- which(directions == 4)
    lin[idx] <- sprintf("border: %s;", lincol[idx])
    css_arguments <- ifelse(directions == 0,
        css_arguments,
        paste(css_arguments, lin, sep = "; "))

    css_arguments <- gsub(";+", ";", css_arguments)
    css_arguments <- gsub(" +;", ";", css_arguments)
    css_arguments <- gsub(" +", " ", css_arguments)
    css_arguments <- trimws(css_arguments)

    sty$css_arguments <- css_arguments
    sty <- sty[sty$css_arguments != "",, drop = FALSE]

    css_table <- data.frame(css_arguments = unique(sty$css_arguments))
    css_table$id <- sapply(seq_len(nrow(css_table)), function(i) get_id(stem = "tinytable_css_"))
    idx <- merge(sty[, c("i", "j", "css_arguments")], css_table)

    if (nrow(idx) > 0) {
        arrays <- list()
        idx <- split(idx, idx$id)
        for (i in seq_along(idx)) {
            id <- idx[[i]]$id[1]
            arr <- sprintf("{ i: %s, j: %s }, ", idx[[i]]$i, idx[[i]]$j)
            arr <- c("          {", " positions: [ ", arr, " ],", " css_id: '", id, "',", "}, ")
            arr <- paste(arr, collapse = "")
            x@table_string <- lines_insert(x@table_string, arr, "tinytable style arrays after", "after")
            entry <- sprintf("      .table td.%s, .table th.%s { %s }", id, id, idx[[i]]$css_arguments[1])
            x@table_string <- lines_insert(x@table_string, entry, "tinytable css entries after", "after")
        }
    }

    return(x)
}


