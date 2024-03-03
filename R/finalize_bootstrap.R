setMethod(
  f = "finalize",
  signature = "tinytable_bootstrap",
  definition = function(x, ...) {

  # class
  cl <- x@bootstrap_class
  if (is.null(cl)) {
    cl <- "table table-borderless"
  }
  out <- sub(
    "$tinytable_BOOTSTRAP_CLASS",
    cl,
    x@table_string,
    fixed = TRUE)

  # Rmarkdown and Quarto load their own bootstrap, which we probably don't want to override
  if (isTRUE(getOption("knitr.in.progress"))) {
    out <- strsplit(out, split = "\n")[[1]]
    out <- out[!grepl("https://cdn.jsdelivr.net/npm/bootstrap", out, fixed = TRUE)]
    out <- paste(out, collapse = "\n")
  }

  x@table_string <- out

  return(x)
})