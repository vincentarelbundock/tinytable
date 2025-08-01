source("helpers.R")
using("tinysnapshot")


# Issue #514: vignette white lines with blue background
dat <- data.frame(1:2, 3:4, 5:6, 7:8)
tab <- tt(dat, theme = "void", colnames = FALSE) |>
    style_tt(
        line = "tblr",
        line_color = "white",
        line_width = 0.5,
        background = "blue",
        color = "white"
    )
t <- expect_table(tab)
expect_snapshot_print(t[["markdown"]], "style-issue514_white_blue.md")
expect_snapshot_print(t[["latex"]], "style-issue514_white_blue.tex")
expect_snapshot_print(t[["typst"]], "style-issue514_white_blue.typ")
expect_snapshot_print(t[["html"]], "style-issue514_white_blue.html")


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
