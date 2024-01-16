source("helpers.R")
using("tinysnapshot")

x <- mtcars[1:4, 1:5]

expect_snapshot_print(
  tt(x, output = "latex", theme = "striped"),
  label = "latex-default")

k <- x
colnames(k) <- NULL
expect_snapshot_print(
  tt(k, output = "latex"),
  label = "latex-nohead")

# Align
expect_snapshot_print(
  tt(x, output = "latex", align = "ccllr"),
  label = "latex-align")

# Themes
expect_snapshot_print(
  tt(x, output = "latex", theme = "striped"),
  label = "latex-theme_striped")

expect_snapshot_print(
  tt(x, output = "latex", theme = "grid"),
  label = "latex-theme_grid")

expect_snapshot_print(
  tt(x, output = "latex", theme = "void"),
  label = "latex-theme_void")

# Styles
expect_snapshot_print(
  tt(x, output = "latex") |> style_tt(i = 1:4, color = "orange"),
  label = "latex-row_color")

expect_snapshot_print(
  tt(x, output = "latex") |> style_tt(j = 1:4, color = "orange"),
  label = "latex-col_color")

expect_snapshot_print(
  tt(x, output = "latex") |> style_tt(i = 1:2, j = 1:4, color = "orange"),
  label = "latex-cell_color")
