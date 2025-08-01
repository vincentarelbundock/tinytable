finalize_theme_void <- function(table) {
  if (isTRUE(table@output == "latex")) {
    s <- table@table_string
    s <- gsub("\\\\toprule|\\\\bottomrule|\\\\midrule", "", s)
    l <- strsplit(s, "\n")[[1]]
    l <- l[which(trimws(l) != "")]
    table@table_string <- paste(l, collapse = "\n")
  } else if (isTRUE(table@output == "markdown")) {
    tab <- table@table_string
    tab <- strsplit(tab, "\n")[[1]]
    tab <- tab[!grepl("^[\\+|-]+$", tab)]
    tab <- tab[!grepl("^[\\+|=]+$", tab)]
    tab <- gsub("|", " ", tab, fixed = TRUE)
    table@table_string <- paste(tab, collapse = "\n")
  }
  return(table)
}

theme_void <- function(x, ...) {
  x <- style_tt(x, finalize = finalize_theme_void)
  x <- theme_tt(x, "bootstrap", class = "table table-borderless")
  return(x)
}
