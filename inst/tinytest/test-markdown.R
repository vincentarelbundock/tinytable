source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")

# markdown without labels
k = iris[1:10,]
colnames(k) <- NULL
tt(k)
expect_snapshot_print(
  tt(k), label = "markdown-nocolnames")

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
        "World" = 8),
      j = list(
        "Foo" = 2:3,
        "Bar" = 4:5))
expect_snapshot_print(tab, label = "markdown-group_tt")



# simple caption
tab <- tt(mtcars[1:3, 1:3], caption = "Blah blah blah") 
expect_snapshot_print(tab, label = "markdown-caption")


# Issue #105
x <- mtcars[1:15,]
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


# Issue #133
tab <- tt(mtcars[1:8, 1:3]) |>
  group_tt(i = list("Hello" = 3, "World" = 6)) |>
  group_tt(j = list("Hello" = 1, "World" = 2:3)) |>
  style_tt(i = 2:4, italic = TRUE) |>
  style_tt(i = 6, strikeout = TRUE) |>
  style_tt(i = 8, bold = TRUE)
expect_snapshot_print(tab, label = "markdown-group_i_style_tt")

# Test bold columns
tab <- tt(mtcars[1:8, 1:5]) |>
  style_tt(j = c(2, 4), bold = TRUE)
expect_snapshot_print(tab, label = "markdown-bold_columns")


# Test replace missing value
tab <- mtcars[1:4, 1:3]
tab[2,3]=NA
tab<-tt(tab)|>
  format_tt(replace = "-")
expect_snapshot_print(tab, label = "markdown-missing_value")


# Test footnote
x <- mtcars[1:3, 1:3]
n <- list("Blah blah", "*" = list(i = 0:1, j = 2, text = "foo bar"))
tab <- tt(x, notes = n)

expect_snapshot_print(tab, label = "markdown-footnote")


# GFM
options(tinytable_print_output = "gfm")
expect_snapshot_print(tt(head(iris)), label = "markdown-gfm_01")
options(tinytable_markdown_hlines = TRUE)
expect_snapshot_print(tt(head(iris)), label = "markdown-gfm_02")
options(tinytable_markdown_hlines = NULL)
options(tinytable_print_output = "markdown")


options(tinytable_print_output = NULL)
