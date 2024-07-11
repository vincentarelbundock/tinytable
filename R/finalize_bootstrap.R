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

  # CSS listeners
  listener_template <- "
   window.addEventListener('load', function () {
       const cellStyles = [
           %s
       ];
       cellStyles.forEach(({coords, class: cssClass}) => {
           styleCell_%s('%s', coords, cssClass);
       });
   });"

  css_template <- "    .table td.%s, .table th.%s { %s }"

  css <- unique(stats::na.omit(x@css))
  css <- css[which(css$bootstrap != ""), ]

  if (nrow(css) > 0) {
    css_rules <- split(css, list(css$i, css$j, css$id))
    css_rules <- Filter(function(z) nrow(z) > 0, css_rules)
    css_rules <- lapply(css_rules, function(z) transform(z, bootstrap = paste(bootstrap)))
    css_rules <- lapply(css_rules, unique)
    css_rules <- do.call(rbind, css_rules)
    css_rules <- split(css_rules, css_rules$id)
    coords <- lapply(css_rules, function(z) sprintf("[%s, %s]", z$i, z$j))
    coords <- lapply(coords, paste, collapse = ", ")

    for (i in seq_along(coords)) {
        co <- sprintf(
          "{coords: [%s], class: '%s'},", 
          coords[[i]],
          names(coords)[i])
        listener <- sprintf(listener_template, co, x@id, names(coords)[i])
        out <- bootstrap_setting(out, listener, component = "cell")
        css_rule <- sprintf(css_template, names(css_rules)[i], names(css_rules)[i], css_rules[[i]]$bootstrap[1])
        out <- bootstrap_setting(out, css_rule, component = "css")
    }
  }

  x@table_string <- out

  for (fn in x@lazy_finalize) {
    x <- fn(x)
  }

  return(x)
})
