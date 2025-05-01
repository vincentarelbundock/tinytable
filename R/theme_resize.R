theme_resize <- function(
  x,
  width = get_option("tinytable_theme_resize_width", 1),
  direction = get_option("tinytable_theme_resize_direction", "down"),
  ...
) {
  fn <- theme_placement_factory(
    horizontal = get_option("tinytable_theme_default_horizontal", "c"),
    latex_float = get_option(
      "tinytable_theme_placement_latex_float",
      default = NULL
    )
  )
  x <- style_tt(x, finalize = fn)

  assert_numeric(width, len = 1, lower = 0.01, upper = 1)
  assert_choice(direction, c("down", "up", "both"))
  # do not change the default theme
  if (identical(x@theme[[1]], "resize")) x@theme <- list("default")
  fn <- function(table) {
    if (!isTRUE(table@output == "latex")) {
      return(table)
    }

    tab <- table@table_string

    if (direction == "both") {
      new <- sprintf("\\resizebox{%s\\linewidth}{!}{", width)
    } else if (direction == "down") {
      new <- sprintf(
        "\\resizebox{\\ifdim\\width>\\linewidth %s\\linewidth\\else\\width\\fi}{!}{",
        width
      )
    } else if (direction == "up") {
      new <- sprintf(
        "\\resizebox{\\ifdim\\width<\\linewidth %s\\linewidth\\else\\width\\fi}{!}{",
        width
      )
    }

    reg <- "\\\\begin\\{tblr\\}|\\\\begin\\{talltblr\\}"
    tab <- lines_insert(tab, regex = reg, new = new, position = "before")

    new <- "}"
    reg <- "\\\\end\\{tblr\\}|\\\\end\\{talltblr\\}"
    tab <- lines_insert(tab, regex = reg, new = new, position = "after")

    table@table_string <- tab

    return(table)
  }

  x <- style_tt(x, finalize = fn)
  return(x)
}
