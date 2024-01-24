
finalize_bootstrap <- function(x) {
    if (meta(x)$output != "html") return(x)
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      "table",
      x,
      fixed = TRUE)
    # Rmarkdown and Quarto load their own bootstrap, which we probably don't want to override
    if (isTRUE(getOption('knitr.in.progress'))) {
      out <- strsplit(out, split = "\n")[[1]]
      out <- out[!grepl("https://cdn.jsdelivr.net/npm/bootstrap", out, fixed = TRUE)]
      out <- paste(out, collapse = "\n")
    }
    return(out)
}