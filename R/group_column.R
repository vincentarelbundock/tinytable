
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
