# TODO: Order of priority by last called
#
init_latex <- function(x) {
  begin <- c(
    "\\begin{table}",
    "\\begin{tblr}[         %% tabularray outer open",
    "]                     %% tabularray outer close",
    "{                     %% tabularray inner open",
    "}                     %% tabularray inner close"
  )
  if (!is.null(colnames(x))) {
    header <- paste(colnames(x), collapse = " & ")
    header <- paste(header, "\\\\")
  } else {
    header <- NULL
  }
  body <- apply(x, 1, paste, collapse = " & ")
  body <- paste(body, "\\\\")
  end <- c(
    "\\end{tblr}",
    "\\end{table}",
    ""
  )
  out <- c(begin, header, body, end)

  tabularray_cols <- rep("Q[]", ncol(x))
  tabularray_rows <- rep("Q[]", nrow(x))
  if (!is.null(header)) { 
    tabularray_rows <- c("Q[]|", tabularray_rows)
  }

  idx <- grep("% tabularray inner open", out)
  out <- c(
    out[1:idx],
    sprintf("colspec={%s},", paste(tabularray_cols, collapse = "")),
    sprintf("rowspec={%s},", paste(tabularray_rows, collapse = "")),
    out[(idx + 1):length(out)]
  )

  out <- paste(out, collapse = "\n")
  attr(out, "ncol") <- ncol(x)
  attr(out, "nrow") <- nrow(x)
  attr(out, "tabularray_cols") <- tabularray_cols
  attr(out, "tabularray_rows") <- tabularray_rows
  class(out) <- c("tinytable_latex", class(out))
  return(out)
}


tabularray_spec <- function(bold,
                            italic,
                            monospace,
                            smallcaps,
                            fg,
                            bg,
                            wd,
                            halign) {
  # Initialize spec
  spec <- ""

  # Flag styles
  args <- list(
    "bold" = list(bold, "\\\\bfseries"),
    "italic" = list(italic, "\\\\textit"),
    "monospace" = list(monospace, "\\\\texttt"),
    "smallcaps" = list(smallcaps, "\\\\scshape")
  )

  font <- ""
  for (n in names(args)) {
    flag <- checkmate::check_flag(args[[n]][[1]])
    if (!isTRUE(flag)) {
      msg <- sprintf("`%s` is not a logical flag.", n)
      stop(msg, call. = FALSE)
    }
    if (isTRUE(args[[n]][[1]])) {
      font <- paste0(font, args[[n]][[2]])
    }
  }
  if (font != "") {
    spec <- paste0(spec, "cmd=", font, ",")
  }

  # String settings: fragile input checks
  args <- list(
    "fg" = fg,
    "bg" = bg,
    "wd" = wd
  )
  for (n in names(args)) {
    flag <- checkmate::check_string(args[[n]], null.ok = TRUE)
    if (!isTRUE(flag)) {
      msg <- sprintf("`%s` is not a string.", n)
      stop(msg, call. = FALSE)
    }
    spec <- paste0(spec, sprintf("%s=%s,", n, args[[n]]))
  }

  # Horizontal alignment
  checkmate::assert_choice(halign, choices = c("c", "l", "r"), null.ok = TRUE)
  if (!is.null(halign)) {
    tmp <- sprintf("halign=%s,", halign)
    spec <- paste0(spec, tmp)
  }

  # Overwrite Q[]/X[] brackets
  spec <- sprintf("[%s]", spec)

  return(spec)
}


style_columns_latex <- function(x,
                                j,
                                halign = NULL,
                                valign = NULL,
                                wd = NULL,
                                fg = NULL,
                                bg = NULL,
                                bold = FALSE,
                                italic = FALSE,
                                monospace = FALSE,
                                smallcaps = FALSE) {

  cols <- attr(x, "tabularray_cols")
  checkmate::assert_class(x, classes = "tinytable_latex")
  checkmate::assert_integerish(j, lower = 1, upper = length(cols), null.ok = FALSE)

  # Get spec from tabularray_spec function
  spec <- tabularray_spec(
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcaps = smallcaps,
    fg = fg,
    bg = bg,
    wd = wd,
    halign = halign
  )

  # Update columns
  cols[j] <- sub("\\[.*\\]", spec, cols[j])
  cols_string <- paste(cols, collapse = "")

  # Write colspec= header
  tab <- strsplit(x, "\n")[[1]]
  idx <- grep("^colspec=\\{", tab)
  tab[idx] <- sprintf("colspec={%s},", cols_string)

  # Re-build table
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- attributes(x)
  attr(tab, "tabularray_cols") <- cols

  return(tab)
}


style_rows_latex <- function(x,
                             i,
                             halign = NULL,
                             valign = NULL,
                             wd = NULL,
                             fg = NULL,
                             bg = NULL,
                             bold = FALSE,
                             italic = FALSE,
                             monospace = FALSE,
                             smallcaps = FALSE) {

  rows <- attr(x, "tabularray_rows")
  checkmate::assert_class(x, classes = "tinytable_latex")
  checkmate::assert_integerish(i, lower = 1, upper = length(rows), null.ok = FALSE)

  # Get spec from tabularray_spec function (same as used in style_columns_latex)
  spec <- tabularray_spec(
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcaps = smallcaps,
    fg = fg,
    bg = bg,
    wd = wd,
    halign = halign
  )

  # Update rows
  rows[i] <- sub("\\[.*\\]", spec, rows[i])
  rows_string <- paste(rows, collapse = "")

  # Write rowspec= header
  tab <- strsplit(x, "\n")[[1]]
  idx <- grep("^rowspec=\\{", tab)
  tab[idx] <- sprintf("rowspec={%s},", rows_string)

  # Re-build table

  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- attributes(x)
  attr(tab, "tabularray_rows") <- rows


  return(tab)
}


style_cells_latex <- function(x,
                              i,
                              j,
                              halign = NULL,
                              valign = NULL,
                              wd = NULL,
                              fg = NULL,
                              bg = NULL,
                              bold = FALSE,
                              italic = FALSE,
                              monospace = FALSE,
                              smallcaps = FALSE) {

  checkmate::assert_class(x, classes = "tinytable_latex")
  checkmate::assert_integerish(i, lower = 1, upper = attr(x, "nrow"), null.ok = FALSE)
  checkmate::assert_integerish(j, lower = 1, upper = attr(x, "ncol"), null.ok = FALSE)

  # Get spec from tabularray_spec function
  spec <- tabularray_spec(
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcaps = smallcaps,
    fg = fg,
    bg = bg,
    wd = wd,
    halign = halign
  )

  # Construct cell header
  cell_header <- sprintf(
    "cell{%s}{%s}={%s},",
    paste(i, collapse = ","),
    paste(j, collapse = ","),
    # for some reason, double-escaping is happening
    gsub("\\\\\\\\", "\\\\", gsub("\\[|\\]", "", spec))
  )

  # Find and replace the cell spec
  tab <- strsplit(x, "\n")[[1]]
  idx <- grep("% tabularray inner close", tab)
  tab <- c(
    tab[1:(idx - 1)],
    cell_header,
    tab[idx:length(tab)]
  )

  # Re-build table
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- attributes(x)

  return(tab)
}


