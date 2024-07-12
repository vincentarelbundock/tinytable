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

  ## I would like to use this fancy listener, but I can't get it to work with multiple style_tt()/group_tt() calls
  # # CSS listeners
  # listener_template <- "
  #  window.addEventListener('load', function () {
  #      const cellStyles = [
  #          %s
  #      ];
  #      cellStyles.forEach(({coords, class: cssClass}) => {
  #          styleCell_%s('%s', coords, cssClass);
  #      });
  #  });"

  css_template <- "    .table td.%s, .table th.%s { %s }"

  css <- unique(stats::na.omit(x@css))
  css <- css[which(css$bootstrap != ""), ]

  if (nrow(css) > 0) {

    css_rules <- css
    css_rules$id <- NULL
    css_rules <- split(css_rules, list(css_rules$i, css_rules$j))
    css_rules <- Filter(function(z) nrow(z) > 0, css_rules)
    css_rules <- lapply(css_rules, function(z) z[rev(seq_len(nrow(z))),])
    css_rules <- lapply(css_rules, unique)
    css_rules <- lapply(css_rules, function(z) transform(z, bootstrap = paste(bootstrap, collapse = " "))[1,])
    css_rules <- do.call(rbind, css_rules)
    id <- unique(css_rules[, "bootstrap", drop = FALSE])
    id$id <- sapply(seq_len(nrow(id)), function(z) sprintf("tinytable_css_%s", get_id()))
    css_rules <- merge(css_rules, id)
    css_rules <- css_rules[order(css_rules$i, css_rules$j),]

    for (ii in seq_len(nrow(css_rules))) {
        listener <- sprintf(
            "window.addEventListener('load', function () { styleCell_%s(%s, %s, '%s') })",
            x@id,
            css_rules$i[[ii]],
            css_rules$j[[ii]],
            css_rules$id[[ii]])
        out <- bootstrap_setting(out, listener, component = "cell")
    }
    css_rules_unique <- unique(css_rules[, c("bootstrap", "id")])
    for (ii in seq_len(nrow(css_rules_unique))) {
        css_rule <- sprintf(css_template, 
                    css_rules_unique$id[[ii]],
                    css_rules_unique$id[[ii]],
                    css_rules_unique$bootstrap[[ii]])
        out <- bootstrap_setting(out, css_rule, component = "css")
    }

  ## I would like to use the fancier listener, but I can't get it to work.
    # css_rules <- split(css_rules, css_rules$id)
    # coords <- lapply(css_rules, function(z) sprintf("[%s, %s]", z$i, z$j))
    # coords <- lapply(coords, paste, collapse = ", ")
  #   for (i in seq_along(coords)) {
  #       co <- sprintf(
  #         "{coords: [%s], class: '%s'},", 
  #         coords[[i]],
  #         names(coords)[i])
  #       listener <- sprintf(listener_template, co, x@id, names(coords)[i])
  #       out <- bootstrap_setting(out, listener, component = "cell")
  #       css_rule <- sprintf(css_template, names(css_rules)[i], names(css_rules)[i], css_rules[[i]]$bootstrap[1])
  #       out <- bootstrap_setting(out, css_rule, component = "css")
  #   }
  #

  }

  x@table_string <- out

  for (fn in x@lazy_finalize) {
    x <- fn(x)
  }

  return(x)
})
