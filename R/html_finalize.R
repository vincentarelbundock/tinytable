setMethod(
  f = "finalize",
  signature = "tinytable_html",
  definition = function(x, ...) {
    # class
    cl <- x@html_class

    out <- sub(
      "$tinytable_HTML_CLASS",
      cl,
      x@table_string,
      fixed = TRUE
    )

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

    # Changing function names to table ID to avoid conflict with other tables functions
    out <- gsub("styleCell_\\w+\\(", paste0("styleCell_", x@id, "("), out)
    out <- gsub("spanCell_\\w+\\(", paste0("spanCell_", x@id, "("), out)

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
          transform(z, html = paste(html, collapse = " "))[1, ]
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
