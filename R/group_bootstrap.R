#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_bootstrap",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
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
)

group_bootstrap_col <- function(x, j, ihead, ...) {
  out <- x@table_string

  out <- strsplit(out, "\\n")[[1]]
  header <- NULL

  miss <- as.list(setdiff(seq_len(ncol(x)), unlist(j)))
  miss <- stats::setNames(miss, rep(" ", length(miss)))
  j <- c(j, miss)

  max_col <- sapply(j, max)
  idx <- order(max_col)
  j <- j[idx]
  jstring <- lapply(seq_along(names(j)), function(k) {
    sprintf(
      '<th scope="col" align="center" colspan=%s data-row="%d" data-col="%d">%s</th>',
      max(j[[k]]) - min(j[[k]]) + 1,
      ihead,
      k - 1,  # 0-based indexing for data-col
      names(j)[k]
    )
  })
  jstring <- paste(unlist(jstring), collapse = "\n")
  jstring <- sprintf("<tr>\n%s\n</tr>", jstring)

  idx <- grep("<thead>", out, fixed = TRUE)[1]
  out <- c(out[seq_len(idx)], jstring, out[(idx + 1):length(out)])

  out <- paste(out, collapse = "\n")

  x@table_string <- out

  x <- style_tt(x, i = ihead, align = "c")

  # midrule on numbered spans (not full columns of body)
  jnames <- names(j)
  jnames <- seq_along(jnames)[trimws(jnames) != ""]
  x <- style_tt(
    x,
    i = ihead,
    j = jnames,
    line = "b",
    line_width = 0.05,
    line_color = "#d3d8dc"
  )

  return(x)
}

group_bootstrap_row <- function(x, i, j, indent = 1, ...) {
  label <- names(i)

  out <- x@table_string

  for (g in seq_along(i)) {
    js <- sprintf(
      "window.addEventListener('load', function () { insertSpanRow(%s, %s, '%s') });",
      # 0-indexing
      i[g] + x@nhead - 1,
      ncol(x),
      names(i)[g]
    )
    out <- lines_insert(out, js, "tinytable span after", "after")
    # out <- bootstrap_setting(out, new = js, component = "cell")
  }

  # need unique function names in case there are
  # multiple tables in one Rmarkdown document
  out <- gsub(
    "insertSpanRow(",
    paste0("insertSpanRow_", get_id(""), "("),
    out,
    fixed = TRUE
  )

  idx <- insert_values(seq_len(nrow(x)), rep(NA, length(i)), i)

  x@table_string <- out

  # if there's a two-level header column multi-span, we want it centered.
  x <- style_tt(x, i = -1, align = "c")

  # do not override meta since we modified it here above
  return(x)
}
