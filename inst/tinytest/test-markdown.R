source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")

# markdown without labels
k = iris[1:10,]
colnames(k) <- NULL
expect_snapshot_print(
  tt(k), label = "markdown-nocolnames")


tab <- tt(mtcars[1:10, 1:5]) |>
    group_tt(
      i = list(
        "Hello" = 3,
        "World" = 8),
      j = list(
        "Foo" = 2:3,
        "Bar" = 4:5))

expect_snapshot_print(tab, label = "markdown-group_tt")


options(tinytable_print_output = NULL)
