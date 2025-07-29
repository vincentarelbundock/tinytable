source("helpers.R")
using("tinysnapshot")

# Issue #507: Test group row styling functionality
tab <- tt(mtcars[1:8, 1:3]) |>
    group_tt(i = list("Hello" = 3, "World" = 6)) |>
    group_tt(j = list("Hello" = 1, "World" = 2:3)) |>
    style_tt(i = 2:4, italic = TRUE) |>
    style_tt(i = 6, strikeout = TRUE) |>
    style_tt(i = 8, bold = TRUE)
t <- expect_table(tab)
expect_snapshot_print(t[["markdown"]], "style-issue507_markdown_styles.md")
expect_snapshot_print(t[["latex"]], "style-issue507_markdown_styles.tex")
expect_snapshot_print(t[["typst"]], "style-issue507_markdown_styles.typ")
expect_snapshot_print(t[["html"]], "style-issue507_markdown_styles.html")
