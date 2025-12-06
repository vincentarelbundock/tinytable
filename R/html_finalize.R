setMethod(
  f = "finalize",
  signature = "tinytable_html",
  definition = function(x, ...) {
    # class
    cl <- x@html_class
    if (!grepl("\\btinytable\\b", cl)) {
      cl <- paste("tinytable", cl)
    }

    out <- sub(
      "$tinytable_HTML_CLASS",
      cl,
      x@table_string,
      fixed = TRUE
    )

    # Handle CSS inclusion - portable vs external
    if (isTRUE(x@html_portable)) {
      # If custom css_rule is provided, use it exclusively (even in portable mode)
      if (!is.null(x@html_css_rule)) {
        css_content <- x@html_css_rule
      } else {
        # For portable HTML, inline the CSS content from the file
        css_file_path <- system.file("tinytable.css", package = "tinytable")
        if (file.exists(css_file_path)) {
          css_content <- readLines(css_file_path, warn = FALSE)
          css_content <- paste(css_content, collapse = "\n")
        } else {
          # Fallback to external if file not found
          css_include <- sprintf(
            '<link rel="stylesheet" href="%s">',
            "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css"
          )
          css_content <- NULL
        }
      }

      if (!is.null(css_content)) {
        css_include <- paste0("<style>\n", css_content, "\n</style>")

        # Also include Bootstrap CSS via external link if using bootstrap engine
        if (identical(x@html_engine, "bootstrap")) {
          bootstrap_css_link <- '<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css">'
          css_include <- paste(bootstrap_css_link, css_include, sep = "\n")
        }
      }
    } else if (is.null(x@html_css_rule)) {
      # Use external CSS file
      css_include <- sprintf(
        '<link rel="stylesheet" href="%s">',
        "https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css"
      )
    } else {
      # Use inline CSS from css_rule
      css_include <- paste0("<style>\n", x@html_css_rule, "\n</style>")
    }

    out <- sub(
      "$tinytable_CSS_INCLUDE",
      css_include,
      out,
      fixed = TRUE
    )

    # Handle JavaScript inclusion - portable vs external
    if (isTRUE(x@html_portable)) {
      # For portable HTML, inline the JS content from the file
      js_file_path <- system.file("tinytable.js", package = "tinytable")
      if (file.exists(js_file_path)) {
        js_content <- readLines(js_file_path, warn = FALSE)
        js_content <- paste(js_content, collapse = "\n")
        js_include <- paste0("<script>\n", js_content, "\n</script>")
      } else {
        # Fallback to external if file not found
        js_include <- '<script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>'
      }

      # Note: For portable mode, we still use external Bootstrap as it's large
      # Users can override with custom css_rule if they need full portability
      if (identical(x@html_engine, "bootstrap")) {
        bootstrap_include <- '   <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js"></script>'
        js_include <- paste(js_include, bootstrap_include, sep = "\n")
      }
    } else {
      # Use external files
      js_include <- '<script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>'
      bootstrap_include <- '   <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js"></script>'

      if (identical(x@html_engine, "bootstrap")) {
        js_include <- paste(js_include, bootstrap_include, sep = "\n")
      }
    }

    out <- sub(
      "$tinytable_JS_INCLUDE",
      js_include,
      out,
      fixed = TRUE
    )

    # Handle custom script inclusion
    if (!is.null(x@html_script)) {
      out <- sub(
        "$tinytable_SCRIPT",
        x@html_script,
        out,
        fixed = TRUE
      )
    } else {
      # Remove placeholder if no script is provided
      out <- lines_drop(
        out,
        "\\$tinytable_SCRIPT",
        fixed = FALSE,
        unique = FALSE
      )
    }

    if (isTRUE(getOption("knitr.in.progress"))) {
      # Rmarkdown and Quarto load their own html, which we probably don't want to override
      out <- lines_drop(
        out,
        "jsdelivr.*bootstrap",
        fixed = FALSE,
        unique = FALSE
      )
      # avoid nesting full HTML page inside an HTML page
      out <- lines_drop_between(
        out,
        regex_start = "<!-- preamble start -->",
        regex_end = "<!-- preamble end -->",
        fixed = TRUE
      )
      out <- lines_drop_between(
        out,
        regex_start = "<!-- postamble start -->",
        regex_end = "<!-- postamble end -->",
        fixed = TRUE
      )
    }

    # Function factory handles table isolation - no need for function name manipulation

    css_template <- "    .table td.%s, .table th.%s { %s }"

    css <- unique(stats::na.omit(x@css))
    css <- css[which(css$html != ""), ]

    if (nrow(css) > 0) {
      css_rules <- css
      css_rules$id <- NULL
      css_rules <- split(css_rules, list(css_rules$i, css_rules$j))
      css_rules <- Filter(function(z) nrow(z) > 0, css_rules)
      css_rules <- lapply(css_rules, function(z) z[rev(seq_len(nrow(z))), ])
      css_rules <- lapply(css_rules, unique)
      css_rules <- lapply(
        css_rules,
        function(z) {
          transform(z, html = paste(z$html, collapse = " "))[1, ]
        })
      css_rules <- do.call(rbind, css_rules)
      id <- unique(css_rules[, "html", drop = FALSE])
      id$id <- sapply(
        seq_len(nrow(id)),
        function(z) sprintf("tinytable_css_%s", get_id())
      )
      css_rules <- merge(css_rules, id, sort = FALSE)
      css_rules <- css_rules[order(css_rules$i, css_rules$j), ]

      for (ii in seq_len(nrow(css_rules))) {
        listener <- sprintf(
          "window.addEventListener('load', function () { styleCell_%s(%s, %s, '%s') })",
          x@id,
          css_rules$i[[ii]],
          css_rules$j[[ii]],
          css_rules$id[[ii]]
        )
        out <- html_setting(out, listener, component = "cell")
      }
      css_rules_unique <- unique(css_rules[, c("html", "id")])
      for (ii in seq_len(nrow(css_rules_unique))) {
        css_rule <- sprintf(
          css_template,
          css_rules_unique$id[[ii]],
          css_rules_unique$id[[ii]],
          css_rules_unique$html[[ii]]
        )
        out <- html_setting(out, css_rule, component = "css")
      }
    }

    x@table_string <- out

    return(x)
  })
