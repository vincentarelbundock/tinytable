source("helpers.R")
using("tinysnapshot")


# Issue #514: vignette white lines with blue background
dat <- data.frame(1:2, 3:4, 5:6, 7:8)
tab <- tt(dat, theme = "empty", colnames = FALSE) |>
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

tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    style_tt("colnames", italic = TRUE) |>
    style_tt("groupj", bold = TRUE)
expect_snapshot_print(tab, "style-groupj_colnames.md")


# Issue #564: Empty which() index should proceed silently
dat <- data.frame(feed = c("fish", "meat", "soy"), weight = c(10, 20, 30))
tab1 <- tt(dat)
tab2 <- tt(dat)

# These should not error when no rows match the condition
expect_silent(style_tt(tab1, i = {
    feed == "cool"
}, bold = TRUE))
expect_silent(style_tt(tab2, i = which(feed == "cool"), bold = TRUE))


# smallcap styling functionality
tab <- tt(head(iris),
    caption = "Motor Trend Car Road Tests",
    notes = "Source: Henderson and Velleman (1981)") |>
    style_tt(i = "colnames", smallcap = TRUE) |>
    style_tt(i = "caption", smallcap = TRUE) |>
    style_tt(i = "notes", smallcap = TRUE) |>
    style_tt(i = 1:3, j = 5, smallcap = TRUE)
t <- expect_table(tab)
expect_snapshot_print(t[["html"]], "style-smallcap.html")
expect_snapshot_print(t[["latex"]], "style-smallcap.tex")
expect_snapshot_print(t[["typst"]], "style-smallcap.typ")
expect_snapshot_print(t[["markdown"]], "style-smallcap.md")


# partial align
tab <- tt(mtcars[1:5, 1:6]) |> style_tt(j = c(2, 4), align = "cr")
t <- expect_table(tab)
expect_snapshot_print(t[["html"]], "style-align_partial.html")
expect_snapshot_print(t[["latex"]], "style-align_partial.tex")
expect_snapshot_print(t[["typst"]], "style-align_partial.typ")
expect_snapshot_print(t[["markdown"]], "style-align_partial.md")


# logical matrix with all FALSE values should not error
testdata <- data.frame(
  names = c('a', 'b', 'c', 'd', 'e'),
  values1 = c(1, 2, 3, 4, 5),
  values2 = c(6, 7, 8, 9, 10)
)
testdata_override <- matrix(FALSE, nrow = nrow(testdata), ncol = ncol(testdata))
result <- testdata |>
  tt() |>
  style_tt(i = testdata_override, background = "red") |>
  save_tt("html")
expect_true(is.character(result))
expect_true(nchar(result) > 0)
