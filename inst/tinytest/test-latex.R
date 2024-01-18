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
  tt(x, output = "latex") |> style_tt(j = 1:5, align = "ccllr"),
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


# Lazy style: group after style is respected
a <- tt(mtcars[1:4, 1:4], output = "latex") |> 
  style_tt(color = "orange", background = "black") |>
  group_tt(j = list("blah" = 1:2, "bar" = 3:4))
b <- tt(mtcars[1:4, 1:4], output = "latex") |> 
  group_tt(j = list("blah" = 1:2, "bar" = 3:4)) |>
  style_tt(color = "orange", background = "black")
expect_snapshot_print(a, label = "latex-group_style_order")
expect_equal(as.character(a), as.character(b))
