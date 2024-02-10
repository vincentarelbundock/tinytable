#' Internal styling function
#'
#' @inheritParams style_tt
#' @keywords internal
#' @noRd
style_bootstrap <- function(x,
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
                            width = NULL,
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


  if (meta(x, "output") != "html") return(x)

  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  # only columns means we also want to style headers 
  if (is.null(i) && !is.null(j)) {
    ival <- c(-1 * rev(seq_len(meta(x)$nhead) - 1), ival)
  }


  # order may be important for recycling 
  settings <- expand.grid(i = ival, j = jval, tabularray = "")
  if (is.null(i) && !is.null(j)) {
    settings <- settings[order(settings$i, settings$j), ]
  }

  # JS 0-indexing
  settings$j <- settings$j - 1
  settings$i <- settings$i - 1 + meta(x, "nhead")


  # settings have a different size for latex, so bootstrap breaks
  vectorize_bootstrap <- function(setting, userinput, string) {
    # simple cases
    if (is.null(userinput) || isFALSE(userinput)) return(setting)
    if(isTRUE(userinput)) return(paste(setting, string))

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
    align_bootstrap <- ifelse(align == "l", "left", align_bootstrap)
    align_bootstrap <- ifelse(align == "r", "right", align_bootstrap)
  } else {
    align_bootstrap <- align
  }

  if (!is.null(alignv)) {
    alignv_bootstrap <- switch(alignv,
                              "t" = "top",
                              "m" = "middle",
                              "b" = "bottom")
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
  settings$bootstrap <- vectorize_bootstrap(settings$bootstrap, width, "width: %s;")
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
  for (row in seq_len(nrow(settings))) {
    # Listener applies the styling to columns
    listener <- "window.addEventListener('load', function () { styleCell_%s(%s, %s, '%s') })"
    listener <- sprintf(listener, settings$id[row], settings$i[row], settings$j[row], settings$id[row])
    out <- bootstrap_setting(out, listener, component = "cell")

    if (rowspan != 1 || colspan != 1) {
      listener <- "window.addEventListener('load', function () { spanCell_%s(%s, %s, %s, %s) })"
      listener <- sprintf(listener, settings$id[row], settings$i[row], settings$j[row], rowspan, colspan)
      out <- bootstrap_setting(out, listener, component = "cell")
    }

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

  if (!is.null(bootstrap_css_rule)) {
    out <- bootstrap_setting(out, bootstrap_css_rule, component = "css")
  }

  if (!is.null(bootstrap_class)) {
    out <- meta(out, "bootstrap_class", bootstrap_class)
  }

  # Changing function names to table ID to avoid conflict with other tables functions 
  out <- gsub("styleCell_\\w+\\(", paste0("styleCell_", meta(x, "id"), "("), out)
  out <- gsub("spanCell_\\w+\\(", paste0("spanCell_", meta(x, "id"), "("), out)

  class(out) <- class(x)
  return(out)
}

