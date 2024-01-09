group_row_latex <- function(x,
                            i,
                            color = NULL,
                            background = NULL,
                            italic = FALSE,
                            bold = FALSE,
                            rule = TRUE) {

  assert_integerish(i)
  assert_flag(rule)

  rule <- if (isTRUE(rule)) "\\midrule" else ""

  if (is.null(names(i))) {
    msg <- "`i` must be a named integer vector."
  }
  label <- names(i)

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
  new <- paste(label, strrep("&", ncol), "\\\\", rule)
  att$body <- c(att$body, new)
  idx <- insert_values(mid, new, i)

  # rebuild table
  tab <- c(top, idx$vec, bot)
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- att
  class(tab) <- class(x)

  tab <- ibStyle(
      tab,
      i = idx$new[is.na(idx$old)] + attr(x, "nhead"), j = 1, italic = italic, bold = bold,
      color = color, background = background, options = tabularrayOptions(c = ncol))

  # we also want to indent the header
  i <- idx$new[!is.na(idx$old)] + attr(x, "nhead")
  if (attr(x, "nhead") > 0) i <- c(1:attr(x, "nhead"), i)
  tab <- ibStyle(tab,
      i = i, j = 1,
      options = tabularrayOptions(
        preto = "\\hspace{1em}",
      ))

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

