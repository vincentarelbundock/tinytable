last_df <- function(x, bycols = c("i", "j")) {
  bycols <- as.list(x[, bycols, drop = FALSE])
  out <- split(x, bycols)
  out <- lapply(out, stats::na.omit)
  out <- Filter(function(k) nrow(k) > 0, out)
  out <- lapply(out, utils::tail, n = 1)
  do.call(rbind, out)
}


expand_styles <- function(x) {
  # 1) Full rectangle of cells
  iseq <- seq_len(nrow(x))
  iseq <- c(-1 * 0:(x@nhead - 1), iseq)
  jseq <- seq_len(ncol(x))
  rect <- expand.grid(i = iseq, j = jseq)

  styles <- x@style
  if (is.null(styles) || !nrow(styles)) {
    return(rect)
  }

  # style property columns (everything except i, j)
  props <- setdiff(names(styles), c("i", "j"))

  style_list <- list()

  for (p in props) {
    # keep only relevant columns, unique rows, and rows with a concrete value for this property
    cols <- intersect(c("i", "j", p), names(styles))
    sub <- unique(styles[, cols, drop = FALSE])
    sub <- sub[!is.na(sub[[p]]), , drop = FALSE]
    if (nrow(sub) == 0) next

    # expand NA i/j
    rows_list <- vector("list", nrow(sub))
    row_style <- list()

    for (r in seq_len(nrow(sub))) {
      if (is.na(sub[r, "i"]) && is.na(sub[r, "j"])) {
        rect_p <- merge(rect, sub[r, p, drop = FALSE], all = TRUE, sort = FALSE)
      } else if (is.na(sub[r, "i"])) {
        rect_p <- merge(rect, sub[r, c("j", p), drop = FALSE], all = TRUE, sort = FALSE)
      } else if (is.na(sub[r, "j"])) {
        rect_p <- merge(rect, sub[r, c("i", p), drop = FALSE], all = TRUE, sort = FALSE)
      } else {
        rect_p <- merge(rect, sub[r, c("i", "j", p), drop = FALSE], all = TRUE, sort = FALSE)
      }
      rect_p <- stats::na.omit(rect_p)
      row_style <- c(row_style, list(rect_p))
    }
    out <- do.call(rbind, row_style)

    if (grepl("line", p)) {
      # there can be multiple line definitions per cell: t, b, l, r
      out <- unique(out)
    } else {
      # last style wins
      out <- last_df(unique(out))
    }
    style_list[[p]] <- out
  }

  style_lines <- style_list[grepl("^line", names(style_list))]
  style_other <- style_list[!grepl("^line", names(style_list))]

  style_lines <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE, sort = FALSE), style_lines)
  style_other <- Reduce(function(d1, d2) merge(d1, d2, all = TRUE, sort = FALSE), style_other)

  out <- list(lines = style_lines, other = style_other)
}
