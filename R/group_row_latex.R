#' @export
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
  # TODO: ignore hlines and midrule
  # both of these are terrible. We need tags in comments probably.
  idx_a <- grep("\\toprule", tab, fixed = TRUE)
  idx_b <- grep("\\bottomrule", tab, fixed = TRUE)
  top <- tab[1:idx_a]
  mid <- tab[(idx_a + 1):idx_b]
  bot <- tab[(idx_b + 1):length(tab)]
  new <- paste(label, strrep("&", ncol), "\\\\", rule)
  idx <- insert_values(mid, new, i)
  tab <- c(top, idx$vec, bot)
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- att
  class(tab) <- class(x)

  tab <- style(
      tab,
      i = idx$new[is.na(idx$old)], j = 1, italic = italic, bold = bold,
      color = color, background = background,
      latex = latexOptions(c = ncol))

  tab <- style(tab,
      i = idx$new[!is.na(idx$old)], j = 1,
      latex = latexOptions(
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

