setMethod(
  f = "finalize",
  signature = "tinytable_bootstrap",
  definition = function(x, ...) {

  # class
  cl <- x@bootstrap_class
  if (is.null(cl) || length(cl) == 0) {
    cl <- "table table-borderless"
  }
  out <- sub(
    "$tinytable_BOOTSTRAP_CLASS",
    cl,
    x@table_string,
    fixed = TRUE)

    if (isTRUE(getOption("knitr.in.progress"))) {
        # Rmarkdown and Quarto load their own bootstrap, which we probably don't want to override
        out <- lines_drop(out, "jsdelivr.*bootstrap", fixed = FALSE, unique = FALSE)
        # avoid nesting full HTML page inside an HTML page
        out <- lines_drop_between(out, 
            regex_start = "<!-- preamble start -->",
            regex_end = "<!-- preamble end -->",
            fixed = TRUE)
        out <- lines_drop_between(out, 
            regex_start = "<!-- postamble start -->",
            regex_end = "<!-- postamble end -->",
            fixed = TRUE)
    }

  # Changing function names to table ID to avoid conflict with other tables functions 
  out <- gsub("styleCell_\\w+\\(", paste0("styleCell_", x@id, "("), out)
  out <- gsub("spanCell_\\w+\\(", paste0("spanCell_", x@id, "("), out)
  
  x@table_string <- out

  for (fn in x@lazy_finalize) {
    x <- fn(x)
  }

  return(x)
})
