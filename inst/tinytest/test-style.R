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


# Issue #512: Many tables with identical output
tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    style_tt("colnames", italic = TRUE) |>
    style_tt(-1, bold = TRUE)
expect_snapshot_print(tab, "style-groupj_colnames.md")
tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    style_tt(0, italic = TRUE) |>
    style_tt(-1, bold = TRUE)
expect_snapshot_print(tab, "style-groupj_colnames.md")
tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    format_tt(0, sprintf = "_%s_") |>
    format_tt(-1, sprintf = "**%s**")
expect_snapshot_print(tab, "style-groupj_colnames.md")
tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    format_tt("colnames", sprintf = "_%s_") |>
    format_tt(-1, sprintf = "**%s**")
expect_snapshot_print(tab, "style-groupj_colnames.md")

exit_file("error: double bold")
tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    style_tt("colnames", italic = TRUE) |>
    style_tt("groupj", bold = TRUE)
expect_snapshot_print(tab, "style-groupj_colnames.md")
