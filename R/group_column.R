
insert_row_latex <- function(x,
                             text = "",
                             position = NULL,
                             italic = TRUE,
                             bold = FALSE) {
  ncol <- attr(x, "ncol")
  att <- attributes(x)
  pos <- 11
  tab <- strsplit(x, split = "\\n")[[1]]
  new <- paste(text, strrep("&", ncol), "\\\\")
  tab <- c(tab[1:(pos - 1)], new, tab[pos:length(tab)])
  tab <- paste(tab, collapse = "\n")
  att$nrow <- att$nrow + 1
  attributes(tab) <- att
  class(tab) <- class(x)
  tab <- tab |>
    style(
      i = 2, j = 1, italic = italic, bold = bold,
      latex = latexOptions(c = ncol)) |>
    style(
      i = 1, j = 1,
      latex = latexOptions(
        preto = "\\hspace{1em}",
      )) |>
    style(
      i = 3:5,
      j = 1,
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

# # Example usage
# vec <- c(10, 20, 30)
# values <- c(25, 15)
# positions <- c(2, 1)
# insert_values(vec, values, positions) |> print()

