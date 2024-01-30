source("helpers.R")
using("tinysnapshot")




# 3 level: markdown
options(tinytable_print_output = "markdown")
x <- mtcars[1:3, 1:5]
tab <- tt(x) |>
    group_tt(j = list("a" = 2:3, "b" = 4:5)) |>
    group_tt(j = list("c" = 1:2, "d" = 3:5)) |>
    group_tt(j = list("e" = 1:3, "f" = 4))
expect_snapshot_print(tab, label = "group_tt-3level_md")
options(tinytable_print_output = NULL)
