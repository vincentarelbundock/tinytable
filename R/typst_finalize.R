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

    # drop the full header block if there are no colnames or group headers
    if (length(x@names) == 0 && nrow(x@group_data_j) == 0) {
      out <- lines_drop_between(
        out,
        regex_start = "// tinytable header start",
        regex_end = "// tinytable header end",
        fixed = TRUE
      )
    }

    if (isTRUE(check_dependency("knitr")) && isTRUE(knitr::pandoc_to("typst"))) {
      lab <- knitr::opts_current$get()[["label"]]
      cap <- knitr::opts_current$get()[["tbl-cap"]]
      # Remove figure environment from template and let Quarto use its own
      if (!is.null(lab) || !is.null(cap)) {
        out <- lines_drop_between(
          out,
          regex_start = "// start preamble figure",
          regex_end = "// end preamble figure",
          fixed = TRUE
        )
        out <- lines_drop(out, regex = "// start preamble figure", fixed = TRUE)
        out <- lines_drop(out, regex = "// end figure", fixed = TRUE)
        out <- sub(" table(", " #table(", out, fixed = TRUE)
      }
    } else {
      # here we kept tinytable's #figure[] so we need to use block[]
      out <- sub("#block", "block", out, fixed = TRUE)
    }

    x@table_string <- out

    return(x)
  })
