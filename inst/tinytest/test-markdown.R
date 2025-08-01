source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")

# markdown without labels
k = iris[1:10, ]
colnames(k) <- NULL
tt(k)
expect_snapshot_print(
  tt(k),
  label = "markdown-nocolnames"
)

# no colnames
tab <- tt(head(iris))
colnames(tab) <- NULL
tab
expect_snapshot_print(tab, label = "markdown-nocolnames_02")

# group rows and columns
tab <- tt(mtcars[1:10, 1:5]) |>
  group_tt(
    i = list(
      "Hello" = 3,
      "World" = 8
    ),
    j = list(
      "Foo" = 2:3,
      "Bar" = 4:5
    )
  )
expect_snapshot_print(tab, label = "markdown-group_tt")

# simple caption
tab <- tt(mtcars[1:3, 1:3], caption = "Blah blah blah")
expect_snapshot_print(tab, label = "markdown-caption")

# Issue #105
x <- mtcars[1:15, ]
i <- list("blah" = 1, "blah" = 3, "blah" = 10)
tab <- tt(x) |> group_tt(i)
expect_snapshot_print(tab, label = "markdown-issue105")

# Issue 127: Large column
test <- data.frame(a = 1, b = 2, c = 3)
tab <- tt(test) |>
  group_tt(j = list("foobar" = 1:2, "hello world" = 3)) |>
  group_tt(j = list("foobar and hello world" = 2:3))
expect_snapshot_print(tab, label = "markdown-group_j_wider_1")

test <- data.frame(a = 1, b = 2, c = 3)
tab <- tt(test) |>
  group_tt(j = list("foobar" = 1:2, "hello world" = 3))
expect_snapshot_print(tab, label = "markdown-group_j_wider_2")

# Test bold columns
tab <- tt(mtcars[1:8, 1:5]) |>
  style_tt(j = c(2, 4), bold = TRUE)
expect_snapshot_print(tab, label = "markdown-bold_columns")

# Test replace missing value
tab <- mtcars[1:4, 1:3]
tab[2, 3] = NA
tab <- tt(tab) |>
  format_tt(replace = "-")
expect_snapshot_print(tab, label = "markdown-missing_value")

# Test footnote
x <- mtcars[1:3, 1:3]
n <- list("Blah blah", "*" = list(i = 0:1, j = 2, text = "foo bar"))
tab <- tt(x, notes = n)

expect_snapshot_print(tab, label = "markdown-footnote")

# GFM
if (!is_local) {
  exit_file("Run on Vincent's machine")
}
options(tinytable_print_output = "gfm")
expect_snapshot_print(tt(head(iris)), label = "markdown-gfm")

# Long group labels - row groups
options(tinytable_print_output = "markdown")
x <- mtcars[1:5, 1:2]
tab <- tt(x) |> group_tt(i = list("AAAAAAAAAAAAAAAAAAAAAAAAAAAA" = 1))
expect_snapshot_print(tab, label = "markdown-long_row_group")

# Long group labels - column groups
x <- mtcars[1:3, 1:4]
tab <- tt(x) |>
  group_tt(j = list("Very Long Column Group Name Here" = 1:2, "Short" = 3:4))
expect_snapshot_print(tab, label = "markdown-long_column_group")

# Long group labels - mixed (both row and column)
x <- mtcars[1:3, 1:4]
tab <- tt(x) |>
  group_tt(
    i = list("SUPER LONG ROW GROUP NAME THAT SHOULD EXPAND TABLE" = 1)
  ) |>
  group_tt(
    j = list("Long Column Group" = 1:2, "Another Long Column Group" = 3:4)
  )
expect_snapshot_print(tab, label = "markdown-long_mixed_groups")

# Multiple long row groups
x <- mtcars[1:6, 1:3]
tab <- tt(x) |>
  group_tt(
    i = list(
      "First Very Long Row Group Name Here" = 1,
      "Second Even Longer Row Group Name That Should Also Expand" = 3,
      "Short" = 5
    )
  )
expect_snapshot_print(tab, label = "markdown-multiple_long_row_groups")

# colspan
x <- head(iris)[1:4, ]
tab <- tt(x) |>
  style_tt(1, 1, italic = TRUE) |>
  style_tt(2, 1, colspan = 3)
expect_snapshot_print(tab, label = "markdown-colspan_basic")

x <- head(iris)[1:4, ]
x[2, 1] <- "Very long content that spans multiple columns"
tab <- tt(x) |>
  style_tt(2, 1, colspan = 3)
expect_snapshot_print(tab, label = "markdown-colspan_long_content")

x <- head(iris)[1:5, ]
tab <- tt(x) |>
  style_tt(2, 1, colspan = 3) |>
  style_tt(4, 2, colspan = 2)
expect_snapshot_print(tab, label = "markdown-colspan_multiple")

# restore
options(tinytable_print_output = NULL)

# colspan test from test-colrowspan.R
options(tinytable_print_output = "markdown")
tab <- aggregate(mpg ~ cyl + am, FUN = mean, data = mtcars)
tab <- tab[order(tab$cyl, tab$am), ]
tab <- tt(tab, digits = 2) |>
  style_tt(i = c(1, 3, 5), j = 1, colspan = 2)
expect_snapshot_print(tab, "colrowspan-markdown_multiple")
options(tinytable_print_output = NULL)
