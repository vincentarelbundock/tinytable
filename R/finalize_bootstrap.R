
finalize_bootstrap <- function(x) {
  if (!isTRUE(meta(x)$output == "html")) {
    return(x)
  }

  # class
  cl <- meta(x, "bootstrap_class")
  if (is.null(cl)) {
    cl <- "table table-borderless"
  }
  out <- sub(
    "$tinytable_BOOTSTRAP_CLASS",
    cl,
    x,
    fixed = TRUE)

  # Rmarkdown and Quarto load their own bootstrap, which we probably don't want to override
  if (isTRUE(getOption("knitr.in.progress"))) {
    out <- strsplit(out, split = "\n")[[1]]
    out <- out[!grepl("https://cdn.jsdelivr.net/npm/bootstrap", out, fixed = TRUE)]
    out <- paste(out, collapse = "\n")
  }

  return(out)
}