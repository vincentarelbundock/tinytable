#' Style a LaTeX and HTML tables
#'
#' @export
style_tt <- function(
  x,
  i, j,
  bold = FALSE,
  italic = FALSE,
  monospace = FALSE,
  color = NULL,
  background = NULL,
  fontsize = NULL,
  width = NULL,
  align = NULL,
  colspan = NULL) {
 
  # no markdown styling
  if (inherits(x, "tinytable_markdown")) return(x)

  if (!inherits(x, "tinytable_tabularray") && !inherits(x, "tinytable_bootstrap")) {
    stop("`x` must be a table produced by `tt()`.", call. = FALSE)
  }

  nhead <- attr(x, "nhead")

  assert_string(color, null.ok = TRUE)
  assert_string(background, null.ok = TRUE)
  assert_string(fontsize, null.ok = TRUE)
  assert_string(width, null.ok = TRUE)
  assert_choice(align, c("c", "l", "r"), null.ok = TRUE)
  assert_flag(bold)
  assert_flag(italic)

  if (!is.null(colspan)) {
    if (missing(j) || missing(i) ||
        (!missing(i) && length(i) != 1) ||
        (!missing(j) && length(j) != 1)) {
      stop("`i` and `j` must be of length 1 when using `colspan`.", call. = FALSE)
    }
    assert_integerish(colspan, len = 1, lower = 2, upper = j + attr(x, "ncol"))
  }


  arguments <- list()

  if (isTRUE(bold)) {
    arguments$bold <- list(
      tabularray = "cmd=\\bfseries",
      bootstrap = "font-weight: bold"
    )
  } 
  if (isTRUE(italic)) {
    arguments$italic <- list(
      tabularray = "cmd=\\textit",
      bootstrap = "font-style: italic"
    )
  } 
  if (isTRUE(monospace)) {
    arguments$monospace <- list(
      tabularray = "cmd=\\texttt",
      bootstrap = "font-family: monospace"
    )
  }
  if (!is.null(align)) {
    arguments$align <- list(
      tabularray = sprintf("halign=%s", align),
      bootstrap = paste("text-align:", switch(align, r = "right", l = "left", c = "center"))
    )
  }
  if (!is.null(colspan)) {
    arguments$colspan <- list(
      tabularray = paste0("c=", colspan),
      bootstrap = ""
    )
  }
  if (!is.null(color)) {
    arguments$color <- list(
      tabularray = paste0("fg=", color),
      bootstrap = paste0("color: ", color)
    )
  }
  if (!is.null(background)) {
    arguments$background <- list(
      tabularray = paste0("bg=", background),
      bootstrap = paste0("background-color: ", background)
    )
  }
  if (!is.null(fontsize)) {
    arguments$fontsize <- list(
      bootstrap = paste("font-size:", fontsize)
    )
  }
  if (!is.null(width)) {
    arguments$width <- list(
      tabularray = sprintf("wd={%s}", width),
      bootstrap = paste("width:%s", width)
    )
  }

  if (inherits(x, "tinytable_bootstrap")) {
    css <- sapply(arguments, function(x) x[["bootstrap"]])
    css <- paste(css, collapse = "; ")
    out <- style_bootstrap(x, i, j, css, colspan = colspan)
    return(out)
  }

  tabularray <- sapply(arguments, function(x) x[["tabularray"]])
  # important for things like colspan
  tabularray <- Filter(function(x) !is.null(x), tabularray)

 
  if (any(c("colspan", "rowspan") %in% names(tabularray))) {
    span <- tabularray[names(tabularray) %in% c("colspan", "rowspan")]
    span <- paste(span, collapse = ",")
    tabularray <- tabularray[!names(tabularray) %in% c("colspan", "rowspan")]
  } else {
    span <- ""
  }

  tabularray <- paste0(paste(tabularray, collapse = ","), ",")

  if (inherits(x, "tinytable_tabularray")) {
    # specified columns or all cells
    if (missing(i)) {
      if (missing(j)) {
        j <- seq_len(attr(x, "ncol"))
      }
      colspec <- sprintf("column{%s}={%s},", paste(j, collapse = ","), tabularray)
      out <- style_tabularray(x, inner = colspec)

    # specified rows
    } else if (missing(j)) {
      # do not style header by default
      rowspec <- sprintf("row{%s}={%s},", paste(i + nhead, collapse = ","), tabularray)
      out <- style_tabularray(x, inner = rowspec)

    # specified cells
    } else {
      cellspec <- sprintf("cell{%s}{%s}={%s}{%s},",
                          # do not style header by default
                          paste(i + nhead, collapse = ","),
                          paste(j, collapse = ","),
                          span,
                          tabularray)
      out <- style_tabularray(x, inner = cellspec)
    }
  }

  return(out)
}
