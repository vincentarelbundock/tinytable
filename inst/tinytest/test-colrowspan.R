source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")
requiet("dplyr")



options(tinytable_print_output = "markdown")
tab <- aggregate(mpg ~ cyl + am, FUN = mean, data = mtcars)
tab <- tab[order(tab$cyl, tab$am), ]
tab <- tt(tab, digits = 2) |>
    style_tt(i = c(1, 3, 5), j = 1, rowspan = 2, colspan = 2)
expect_snapshot_print(tab, "colrowspan-markdown_multiple")
options(tinytable_print_output = NULL)

