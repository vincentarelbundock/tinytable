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


# Caption and notes styles must not duplicate the preceding cell style frame
tab <- tt(
    data.frame(a = 1:2),
    caption = "Caption",
    notes = "Note") |>
    style_tt(i = 1, line = "b", line_color = "red", line_width = 0.123) |>
    style_tt(i = "caption", bold = TRUE) |>
    style_tt(i = "notes", italic = TRUE)
tab <- tinytable:::build_tt(tab, output = "typst")
user_lines <- tab@style_lines[
    tab@style_lines$line_color == "red" & tab@style_lines$line_width == 0.123,
    ,
    drop = FALSE]
expect_equal(nrow(user_lines), 1L)
line_matches <- gregexpr(
    'stroke: 0.123em + rgb("#FF0000")',
    tab@table_string,
    fixed = TRUE)[[1]]
expect_equal(sum(line_matches > 0L), 1L)


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


# Issue #647: Conditional styling with output argument
tab <- data.frame(A = 1:2, B = 3:4) |>
  tt() |>
  style_tt(bold = TRUE, output = "typst") |>
  style_tt(italic = TRUE, output = "html")
t <- expect_table(tab, formats = c("html", "typst", "latex"))
expect_snapshot_print(t[["html"]], "style-output_conditional.html")
expect_snapshot_print(t[["typst"]], "style-output_conditional.typ")
expect_snapshot_print(t[["latex"]], "style-output_conditional.tex")


# Deprecated shims must warn AND take effect (tabularray_inner/outer,
# html_class, html_css_rule). Regression: `theme_latex()` was called without
# `x`, and html shim results were discarded.
tab <- style_tt(tt(mtcars[1:3, 1:3]), tabularray_inner = "rowsep=8pt")
expect_warning(build_tt(tab, "latex"), pattern = "tabularray_inner")
b <- suppressWarnings(build_tt(tab, "latex"))
expect_true(grepl("rowsep=8pt", b@table_string, fixed = TRUE))

tab <- style_tt(tt(mtcars[1:3, 1:3]), tabularray_outer = "baseline=T")
expect_warning(build_tt(tab, "latex"), pattern = "tabularray_outer")
b <- suppressWarnings(build_tt(tab, "latex"))
expect_true(grepl("baseline=T", b@table_string, fixed = TRUE))

tab <- style_tt(tt(mtcars[1:3, 1:3]), html_class = "table-dark-x")
expect_warning(build_tt(tab, "html"), pattern = "html_class")
b <- suppressWarnings(build_tt(tab, "html"))
expect_true(grepl("table-dark-x", b@table_string, fixed = TRUE))

tab <- style_tt(tt(mtcars[1:3, 1:3]), html_css_rule = ".mystyle { color: pink; }")
expect_warning(build_tt(tab, "html"), pattern = "html_css_rule")
b <- suppressWarnings(build_tt(tab, "html"))
expect_true(grepl(".mystyle { color: pink; }", b@table_string, fixed = TRUE))


# Empty row selection with align must be a silent no-op, not a build error
# Regression: "replacement has 1 row, data has 0"
tab <- style_tt(tt(mtcars[1:3, 1:3]), i = integer(0), align = "c")
b <- build_tt(tab, "html")
expect_inherits(b, "tinytable")


# Logical-matrix `i` with per-cell style vectors: values recycle over TRUE
# cells in column-major order (which(i, arr.ind = TRUE))
d <- data.frame(a = 1:3, b = 4:6)
m <- matrix(FALSE, 3, 2)
m[2, 1] <- TRUE
m[1, 2] <- TRUE
b <- build_tt(style_tt(tt(d), i = m, color = c("red", "blue")), "html")
sty <- b@style[!is.na(b@style$color), ]
expect_equal(nrow(sty), 2)
expect_equal(sty$color[sty$i == 2 & sty$j == 1], "red")
expect_equal(sty$color[sty$i == 1 & sty$j == 2], "blue")

# length-1 values still recycle over all TRUE cells
b <- build_tt(style_tt(tt(d), i = m, color = "green"), "html")
sty <- b@style[!is.na(b@style$color), ]
expect_equal(sty$color[sty$i == 2 & sty$j == 1], "green")
expect_equal(sty$color[sty$i == 1 & sty$j == 2], "green")

# wrong vector length errors informatively (valid lengths: 1 or #TRUE cells)
expect_error(
  build_tt(style_tt(tt(d), i = m, color = c("a", "b", "c")), "html"),
  pattern = "one of these lengths: 1, 2"
)

# logical matrix with wrong dimensions must error, not degrade silently
m2 <- matrix(TRUE, 2, 2)
expect_error(
  build_tt(style_tt(tt(d), i = m2, bold = TRUE), "html"),
  pattern = "dimensions must match"
)

# align is ignored (with a warning) when `i` is a logical matrix
expect_warning(
  build_tt(style_tt(tt(d), i = m, align = "r"), "html"),
  pattern = "logical matrix"
)


# Value-to-column mapping honors USER-specified `j` order, consistent with `i`
# Regression: j = c(3, 1) used to sort and put the first value on column 1
d3 <- data.frame(a = 1:2, b = 3:4, c = 5:6)
b <- build_tt(style_tt(tt(d3), j = c(3, 1), background = c("red", "blue")), "html")
sty <- b@style[!is.na(b@style$background), ]
expect_true(all(sty$background[sty$j == 3] == "red"))
expect_true(all(sty$background[sty$j == 1] == "blue"))

# character `j` also honors user order
b <- build_tt(style_tt(tt(d3), j = c("c", "a"), background = c("red", "blue")), "html")
sty <- b@style[!is.na(b@style$background), ]
expect_true(all(sty$background[sty$j == 3] == "red"))
expect_true(all(sty$background[sty$j == 1] == "blue"))

# `i` user-order behavior is unchanged
b <- build_tt(style_tt(tt(d3), i = c(2, 1), j = 1, background = c("red", "blue")), "html")
sty <- b@style[!is.na(b@style$background), ]
expect_true(all(sty$background[sty$i == 2] == "red"))
expect_true(all(sty$background[sty$i == 1] == "blue"))

# Compound line directions do not duplicate non-line properties in @style
b <- build_tt(style_tt(tt(d3, theme = "empty"), i = 1, j = 1, line = "tb", background = "red"), "html")
expect_equal(sum(!is.na(b@style$background)), 1)
expect_equal(sum(!is.na(b@style$line)), 2)
