theme_bootstrap <- function(x, ...) {
  x <- style_tt(x, finalize = theme_void_fn) # only affects LaTeX

  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option(
      "tinytable_theme_placement_latex_float",
      default = NULL
    )
  )
  x <- style_tt(x, finalize = fn)

  x <- style_tt(x, finalize = fn)

  fn <- function(table) {
    if (isTRUE(table@output %in% c("latex", "typst"))) {
      table <- style_tt(
        table,
        i = 0:nrow(x),
        line = "bt",
        line_width = 0.05,
        line_color = "#C0C0C0"
      )
    } else if (isTRUE(table@output == "markdown")) {
      tab <- table@table_string
      tab <- strsplit(tab, "\n")[[1]]
      tab <- tab[!grepl("^[\\+|-]+$", tab)]
      tab <- gsub("|", " ", tab, fixed = TRUE)
      table@table_string <- paste(tab, collapse = "\n")
    }
    return(table)
  }
  x <- style_tt(x, finalize = fn)

  x <- style_tt(x, bootstrap_class = "table")

  return(x)
}
