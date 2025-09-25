source("helpers.R")
using("tinysnapshot")

# Bug: \begin{table}[H][H]
options(tinytable_latex_placement = "H")
x <- mtcars[1:4, 1:4]
tab <- tt(x) |> theme_latex(resize_width = .9, resize_direction = "down")
tab@output <- "latex"
expect_snapshot_print(tab, label = "theme-placement_options_no_doubling")
options(tinytable_latex_placement = "H")

# Issue #206: resize with footnote
k <- data.frame(X = 1) |>
  tt(note = "abc") |>
  theme_latex(resize_width = 0.9, resize_direction = "down") |>
  save_tt("latex")
expect_inherits(k, "character")

# Issue ##439: function theme breaks HTML
theme_mitex <- function(x, ...) {
  fn <- function(table) {
    if (isTRUE(table@output == "typst")) {
      table@table_string <- gsub(
        "\\$(.*?)\\$",
        "#mitex(`\\1`)",
        table@table_string
      )
    }
    return(table)
  }
  x <- style_tt(x, finalize = fn)
  return(x)
}
tab <- data.frame(Math = c("$\\alpha$", "$a_{it}$", "$e^{i\\pi} + 1 = 0$")) |>
  tt(theme = theme_mitex) |>
  save_tt("html")
expect_inherits(tab, "character")


# Issue #460: rowhead is not inserted in LaTeX
tmp <- rbind(mtcars, mtcars)[, 1:6]
cap <- "A long 80\\% width table with repeating headers."
tab <- tt(tmp, width = .8, caption = cap) |>
  theme_latex(multipage = TRUE, rowhead = 1) |>
  save_tt("latex")
expect_true(grepl("rowhead=1", tab))


# theme_tt() deprecation warning
expect_warning(theme_tt(tt(head(iris)), "striped"), pattern = "deprecated")


# Issue #531: style_tt() overrides triped theme
iris_dt <- do.call(rbind, by(iris, ~Species, head, 2))
cap <- "Stripes override purple and teal, but not yellow."
t = tab <- tt(iris_dt, theme = "empty", caption = cap) |>
  style_tt(i = 1:2, j = 1, background = "#4B0055") |>
  style_tt(i = 3:4, j = 1, background = "#009B95") |>
  theme_striped() |>
  style_tt(i = 5:6, j = 1, background = "#FDE333")
expect_snapshot_print(
  print_html(tab),
  "theme-issue531_style_colors_override_stripes.html")
