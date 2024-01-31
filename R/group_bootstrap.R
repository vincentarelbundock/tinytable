
group_bootstrap <- function(x, i, j, indent = 1, ...) {
  out <- x
  # columns first to count headers properly
  if (!is.null(j)) {
    out <- group_bootstrap_col(out, j = j, ...)
  }
  if (!is.null(i)) {
    out <- group_bootstrap_row(out, i = i, j = j, indent = indent, ...)
  }
  return(out)
}


group_bootstrap_col <- function(x, j, ihead, ...) {

  m <- meta(x)

  out <- strsplit(x, "\\n")[[1]]
  header <- NULL


  miss <- as.list(setdiff(seq_len(m$ncols), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)

  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  jstring <- lapply(names(j), function(n) sprintf(
    '<th scope="col" align="center" colspan=%s>%s</th>',
    max(j[[n]]) - min(j[[n]]) + 1, n))
  jstring <- paste(unlist(jstring), collapse = "\n")
  jstring <- sprintf("<tr>\n%s\n</tr>", jstring)

  idx <- grep("<thead>", out, fixed = TRUE)[1]
  out <- c(out[seq_len(idx)], jstring, out[(idx + 1):length(out)])

  out <- paste(out, collapse = "\n")

  class(out) <- class(x)
  attr(out, "tinytable_meta") <- m

  out <- style_bootstrap(out, i = ihead, align = "c")

  # midrule on numbered spans (not full columns of body)
  jnames <- names(j)
  jnames <- seq_along(jnames)[trimws(jnames) != ""]
  out <- style_bootstrap(out, i = ihead, j = jnames, line = "b", line_width = .05, line_color = "#d3d8dc")

  return(out)
}


group_bootstrap_row <- function(x, i, j, indent = 1, ...) {
  label <- names(i)

  m <- meta(x)

  # reverse order is important
  i <- rev(sort(i))


  tab <- strsplit(x, "\\n")[[1]]
  out <- x

  for (g in seq_along(i)) {
    js <- sprintf(
      "window.addEventListener('load', function () { insertSpanRow(%s, %s, '%s') });",
      # 0-indexing
      i[g] + m$nhead - 1,
      m$ncols,
      names(i)[g])
    out <- bootstrap_setting(out, new = js, component = "cell")
  }

  # add rows to attributes BEFORE style_tt
  out <- meta(out, "nrows", m$nrows + length(label))

  # need unique function names in case there are
  # multiple tables in one Rmarkdown document
  out <- gsub(
    "insertSpanRow(",
    paste0("insertSpanRow_", get_id(""), "("),
    out,
    fixed = TRUE)

  idx <- insert_values(seq_len(m$nrows), rep(NA, length(i)), i)
  idx_old <- idx$new[!is.na(idx$old)]
  idx_new <- idx$new[is.na(idx$old)]

  # limit index ot number of rows to avoid styling header or footer
  idx_old <- idx_old[idx_old <= meta(x)$nrows]

  # should not be style_tt, because we already have a string bootstrap table at this stage
  out <- style_bootstrap(out, i = idx_old, j = 1, indent = indent)

  # if there's a two-level header column multi-span, we want it centered.
  out <- style_bootstrap(out, i = -1, align = "c")

  dots <- list(...)
  dots[["j"]] <- NULL
  if (length(dots) > 0) {
    args <- c(list(x = out, i = idx$new[is.na(idx$old)]), dots)
    out <- do.call(style_tt, args)
  }

  # do not override meta since we modified it here above
  return(out)
}
