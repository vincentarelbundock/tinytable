source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")

# markdown without labels
k = iris[1:10,]
colnames(k) <- NULL
expect_snapshot_print(
  tt(k), label = "markdown-nocolnames")

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




options(tinytable_print_output = NULL)
