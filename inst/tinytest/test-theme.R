source("helpers.R")
using("tinysnapshot")

# Bug: \begin{table}[H][H]
options(tinytable_theme_placement_latex_float = "H")
x <- mtcars[1:4, 1:4]
tab <- tt(x) |> theme_tt("resize", width = .9)
tab@output <- "latex"
expect_snapshot_print(tab, label = "theme-placement_options_no_doubling")
options(tinytable_theme_placement_latex_float = "H")

# Issue #206: resize with footnote
k <- data.frame(X = 1) |>
  tt(note = "abc") |>
  theme_tt(theme = "resize", width = 0.9) |>
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
  x <- theme_tt(x, theme = "default")
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
  theme_tt("multipage", rowhead = 1) |>
  save_tt("latex")
expect_true(grepl("rowhead=1", tab))

