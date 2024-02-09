source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")
requiet("dplyr")



options(tinytable_print_output = "markdown")
tab <- mtcars |>
    summarize(mpg = mean(mpg), .by = c("cyl", "am")) |>
    arrange(cyl, am)
tab <- tt(tab, digits = 2) |>
    style_tt(i = 1, j = 1, rowspan = 2, colspan = 2) |>
    style_tt(i = 3, j = 1, rowspan = 2, colspan = 2) |>
    style_tt(i = 5, j = 1, rowspan = 2, colspan = 2)
expect_snapshot_print(tab, "colrowspan-markdown_multiple")
options(tinytable_print_output = NULL)