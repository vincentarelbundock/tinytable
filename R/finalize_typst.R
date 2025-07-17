setMethod(
  f = "finalize",
  signature = "tinytable_typst",
  definition = function(x, ...) {
    out <- x@table_string

    cap <- x@caption
    if (length(cap) == 1) {
      out <- sub(
        "$TINYTABLE_TYPST_CAPTION",
        sprintf("caption: %s,", cap),
        out,
        fixed = TRUE
      )
    } else {
      out <- sub("$TINYTABLE_TYPST_CAPTION", "", out, fixed = TRUE)
    }

    if (length(x@names) == 0) {
      out <- lines_drop_between(
        out,
        regex_start = "// tinytable header start",
        regex_end = "// tinytable header end",
        fixed = TRUE
      )
    }

    # Quarto cross-references
    if (isTRUE(check_dependency("knitr"))) {
      quarto_caption <- isTRUE(knitr::pandoc_to("typst")) &&
        isFALSE(getOption("tinytable_quarto_figure", default = FALSE)) &&
        (!is.null(knitr::opts_current$get()[["label"]]) ||
          !is.null(knitr::opts_current$get()[["tbl-cap"]]))
      if (quarto_caption) {
        out <- lines_drop_between(
          out,
          regex_start = "// start preamble figure",
          regex_end = "// end preamble figure",
          fixed = TRUE
        )
        out <- lines_drop(out, regex = "// start preamble figure", fixed = TRUE)
        out <- lines_drop(out, regex = "// end figure", fixed = TRUE)
        out <- sub(" table(", " #table(", out, fixed = TRUE)
      } else {
        out <- sub("#block", "block", out, fixed = TRUE)
      }
    }

    x@table_string <- out

    for (fn in x@lazy_finalize) {
      x <- fn(x)
    }

    return(x)
  }
)
