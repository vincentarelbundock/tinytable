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
