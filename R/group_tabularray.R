#' tinytable S4 method
#'
#' @keywords internal
setMethod(
  f = "group_eval",
  signature = "tinytable_tabularray",
  definition = function(x, i = NULL, j = NULL, indent = 1, ...) {
    # columns first to count headers properly
    x <- group_tabularray_col(x, j, ...)
    x <- group_tabularray_row(x, i, indent)
    return(x)
  })


group_tabularray_col <- function(x, j, ihead, ...) {
  if (is.null(j)) {
    return(x)
  }

  out <- strsplit(x@table_string, split = "\\n")[[1]]

  header <- rep("", ncol(x))
  for (n in names(j)) {
    header[min(j[[n]])] <- n
  }
  header <- paste(header, collapse = " & ")

  # \toprule -> \midrule
  midr <- sapply(j, function(x) sprintf("\\cmidrule[lr]{%s-%s}", min(x), max(x)))
  header <- paste(header, "\\\\", paste(midr, collapse = ""))

  idx <- max(c(
    grep("% tabularray inner close", out),
    grep("\\toprule", out, fixed = TRUE)
  ))

  out <- c(
    out[1:idx],
    # empty lines can break latex
    trimws(header),
    out[(idx + 1):length(out)])
  out <- paste(out, collapse = "\n")

  # rebuild including meta before style_tt
  x@table_string <- out

  for (k in seq_along(j)) {
    z <- min(j[[k]])
    args <- list(
      tt_build_now = TRUE,
      x = x,
      i = ihead,
      j = z,
      align = "c",
      colspan = max(j[[k]]) - min(j[[k]]) + 1)
    x <- do.call(style_tt, args)
  }

  return(x)
}


group_tabularray_row <- function(x, i, indent) {
  if (is.null(i)) {
    return(x)
  }

  # reverse order is important
  i <- rev(sort(i))

  if (is.null(names(i))) {
    msg <- "`i` must be a named integer vector."
    stop(msg, call. = FALSE)
  }
  label <- names(i)

  tab <- strsplit(x@table_string, "\\n")[[1]]

  # store the original body lines when creating the table, and use those to guess the boundaries.
  # a hack, but probably safer than most regex approaches I can think of.
  body_min <- max(grep("TinyTableHeader|toprule|inner close", tab)) + 1
  body_max <- min(grep("bottomrule|end.tblr", tab))
  body <- body_min:body_max
  top <- tab[1:(min(body) - 1)]
  mid <- tab[min(body):max(body)]
  bot <- tab[(max(body) + 1):length(tab)]

  # separator rows
  # add separator rows so they are treated as body in future calls
  new <- paste(label, strrep("&", ncol(x) - 1), "\\\\")
  x@body <- c(x@body, new)
  idx <- insert_values(mid, new, i)

  # rebuild table
  tab <- c(top, idx$vec, bot)
  tab <- paste(tab, collapse = "\n")

  cellspec <- sprintf(
    "cell{%s}{%s}={%s}{%s},",
    idx$new[is.na(idx$old)] + x@nhead,
    1,
    paste0("c=", ncol(x)),
    ""
  )
  cellspec <- paste(cellspec, collapse = "")
  tab <- tabularray_insert(tab, content = cellspec, type = "inner")

  # we also want to indent the header
  i <- idx$new[!is.na(idx$old)] + x@nhead
  cellspec <- sprintf("cell{%s}{%s}={%s},\n", i, 1, sprintf("preto={\\hspace{%sem}}", indent))
  cellspec <- paste(cellspec, collapse = "")
  tab <- tabularray_insert(tab, content = cellspec, type = "inner")

  x@table_string <- tab

  return(x)
}



insert_values <- function(vec, values, positions) {
  if (length(values) != length(positions)) {
    stop("The length of values and positions must be the same")
  }

  # Sort the positions in decreasing order along with their corresponding values
  ord <- order(positions, decreasing = TRUE)
  values <- values[ord]
  positions <- positions[ord]

  # Create a vector of indices for the original vector
  original_indices <- seq_along(vec)


  # Insert values and update indices
  for (i in seq_along(values)) {
    vec <- append(vec, values[i], after = positions[i] - 1)
    original_indices <- append(original_indices, NA, after = positions[i] - 1)
  }

  # Return the extended vector and the original indices vector
  return(data.frame(vec = vec, old = original_indices, new = seq_along(vec)))
}
