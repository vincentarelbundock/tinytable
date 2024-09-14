#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_typst",
  definition = function(x, i = NULL, j = NULL, indent = 0, ...) {
    out <- x

    if (!is.null(i)) {
      out <- group_typst_row(out, i, indent)
    }

    if (!is.null(j)) {
      out <- group_typst_col(out, j, ...)
    }

    return(out)
  })


group_typst_row <- function(x, i, indent, ...) {
  tab <- x@table_string
  tab <- strsplit(tab, split = "\\n")[[1]]
  body_min <- utils::head(grep("tinytable cell content after", tab), 1) + x@nhead
  body_max <- utils::head(grep("end table", tab), 1) - 1
  body <- body_min:body_max
  top <- tab[1:(body_min - 1)]
  mid <- tab[body_min:body_max]
  mid <- mid[mid != ""]
  bot <- tab[(body_max + 1):length(tab)]
  for (idx in rev(seq_along(i))) {
    mid <- append(mid,
      sprintf("table.cell(colspan: %s)[%s],", ncol(x), names(i)[idx]),
      after = i[idx] - 1)
  }
  tab <- c(top, mid, bot)
  tab <- paste(tab, collapse = "\n")
  x@table_string <- tab
  idx_new <- i + seq_along(i) + x@nhead - 2
  idx_old <- setdiff(seq_len(nrow(x)), idx_new)
  x <- style_tt(x, idx_old, indent = indent)
  return(x)
}


group_typst_col <- function(x, j, ihead, ...) {
  out <- x@table_string
  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)
  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  lab <- names(j)
  len <- lengths(j)
  col <- ifelse(
    trimws(lab) == "",
    sprintf("[%s],", lab),
    sprintf("table.cell(stroke: (bottom: .05em + black), colspan: %s, align: center)[%s],", len, lab))
  col <- paste(col, collapse = "")
  out <- lines_insert(out, col, "repeat: true", "after")
  if (!any(grepl("column-gutter", out))) {
    out <- lines_insert(out, "    column-gutter: 5pt,", "// tinytable table start", "after")
  }

  x@table_string <- out

  # # midrule
  # jrule <- lapply(names(j), function(n) if (trimws(n) != "") j[[n]])
  # jrule <- Filter(function(k) !is.null(k), jrule)
  # for (jr in jrule) {
  #   # 0 indexing
  #   x <- style_eval(x, i = ihead, j = jr, line = "b", line_width = .05, midrule = TRUE)
  # }

  return(x)
}
