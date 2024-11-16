setMethod(
  f = "finalize",
  signature = "tinytable_typst",
  definition = function(x, ...) {

  out <- x@table_string
  out <- sub("$TINYTABLE_TYPST_NROW", nrow(x), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NCOL", ncol(x), out, fixed = TRUE)
  out <- sub("$TINYTABLE_TYPST_NHEAD", x@nhead, out, fixed = TRUE)


  cap <- x@caption
  if (length(cap) == 1) {
    out <- sub("$TINYTABLE_TYPST_CAPTION", sprintf("caption: [%s],", cap), out, fixed = TRUE)
  } else {
    out <- sub("$TINYTABLE_TYPST_CAPTION", "", out, fixed = TRUE)
  }


  # Quarto cross-references
  if (isTRUE(check_dependency("knitr"))) {
    flag_typst <- isTRUE(knitr::pandoc_to("typst"))
    flag_option <- isTRUE(getOption("tinytable_quarto_figure", default = FALSE))
    flag_chunk_label <- isTRUE(grepl("^tbl-", knitr::opts_current$get()[["label"]]))
    flag_chunk_caption <- !is.null(knitr::opts_current$get()[["tbl-cap"]])
    quarto_caption <- flag_typst && flag_option && (!flag_chunk_label || !flag_chunk_caption) 
    if (quarto_caption) {
       out <- lines_drop_between(out, 
                    regex_start = "// start figure preamble",
                    regex_end = "// end figure preamble",
                    fixed = TRUE)
       out <- lines_drop(out, regex = "// start figure preamble", fixed = TRUE)
       out <- lines_drop(out, regex = "// end figure", fixed = TRUE)
       out <- lines_drop(out, regex = "// start block", fixed = TRUE)
       out <- lines_drop(out, regex = "// end block", fixed = TRUE)
       out <- sub(" table(", " #table(", out, fixed = TRUE)
    }
  }

  x@table_string <- out

  for (fn in x@lazy_finalize) {
    x <- fn(x)
  }

  return(x)
})
