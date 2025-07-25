source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")

# issue #98: Grid alignment
x <- mtcars[1:4, 1:5]
tab <- tt(x) |> group_tt(j = list(a = 2:3, b = 4:5))
expect_snapshot_print(tab, label = "docx-issue98_01")

x[1, 1] <- "$\\sigma$"
expect_snapshot_print(tt(x), label = "docx-issue98_02")

colnames(x)[1] <- "$\\sigma$"
expect_snapshot_print(tt(x), label = "docx-issue98_03")

x[1, 1] <- "1"
expect_snapshot_print(tt(x), label = "docx-issue98_04")

options(tinytable_print_output = NULL)
