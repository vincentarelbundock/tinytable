group_tabularray <- function(x, i, j, indent, ...) {
  if (!is.null(i)) {
    out <- group_tabularray_row(x, i, indent, ...)
  } else {
    out <- group_tabularray_col(x, j, ...)
  }
  return(out)
}




group_tabularray_col <- function(x, j, ...) {

  dots <- list(...)

  att <- attributes(x)

  out <- strsplit(x, split = "\\n")[[1]]

  header <- rep("", attr(x, "ncol"))
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

  out <- c(out[1:idx],
           # empty lines can break latex
           trimws(header),
           out[(idx + 1):length(out)])
  out <- paste(out, collapse = "\n")

  attributes(out) <- att
  class(out) <- class(x)

  for (k in seq_along(j)) {
    z <- min(j[[k]])
    idx <- 1 - attr(x, "nhead")
    args <- list(x = out,
                 i = idx,
                 j = z,
                 colspan = max(j[[k]]) - min(j[[k]]) + 1)
    if (!"halign" %in% names(dots)) {
      args["align"] <- "c" 
    }
    args <- c(args, dots)
    out <- do.call(style_tt, args)
  }

  return(out)

}


group_tabularray_row <- function(x, i, indent, ...) {
  if (is.null(names(i))) {
    msg <- "`i` must be a named integer vector."
  }
  label <- names(i)

  ## we don't appear to need to reverse in tabularray
  # i <- rev(sort(i))

  ncol <- attr(x, "ncol")
  att <- attributes(x)
  att$nrow <- att$nrow + length(label)
  tab <- strsplit(x, "\\n")[[1]]

  # store the original body lines when creating the table, and use those to guess the boundaries.
  # a hack, but probably safer than most regex approaches I can think of.
  body <- which(tab %in% attr(x, "body"))
  top <- tab[1:(min(body) - 1)]
  mid <- tab[min(body):max(body)]
  bot <- tab[(max(body) + 1):length(tab)]

  # separator rows
  # add separator rows so they are treated as body in future calls
  new <- paste(label, strrep("&", ncol), "\\\\")
  att$body <- c(att$body, new)
  idx <- insert_values(mid, new, i)

  # rebuild table
  tab <- c(top, idx$vec, bot)
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- att
  class(tab) <- class(x)

  # add rows to attributes BEFORE style_tt
  attr(tab, "nrow") <- attr(tab, "nrow") + length(label)

  cellspec <- sprintf("cell{%s}{%s}={%s}{%s},",
    idx$new[is.na(idx$old)] + attr(x, "nhead"),
    1,
    paste0("c=", ncol),
    ""
  )
  cellspec <- paste(cellspec, collapse = "")
  tab <- style_tabularray(tab, inner = cellspec)

  # we also want to indent the header
  i <- idx$new[!is.na(idx$old)] + attr(x, "nhead")
  if (attr(x, "nhead") > 0) i <- c(1:attr(x, "nhead"), i)
  cellspec <- sprintf("cell{%s}{%s}={%s},", i, 1, sprintf("preto={\\hspace{%sem}}", indent))
  cellspec <- paste(cellspec, collapse = "")
  tab <- style_tabularray(tab, inner = cellspec)

  dots <- list(...)
  if (length(dots) > 0) {
    args <- c(list(x = tab, i = idx$new[is.na(idx$old)]), dots)
    tab <- do.call(style_tt, args)
  }

  return(tab)
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
  original_indices <- 1:length(vec)


  # Insert values and update indices
  for (i in 1:length(values)) {
    vec <- append(vec, values[i], after = positions[i] - 1)
    original_indices <- append(original_indices, NA, after = positions[i] - 1)
  }
  
  # Return the extended vector and the original indices vector
  return(data.frame(vec = vec, old = original_indices, new = seq_along(vec)))
}

