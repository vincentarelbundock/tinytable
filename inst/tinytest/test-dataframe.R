source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "dataframe")

x <- head(iris)
tab <- tt(x) |>
    group_tt(i = list("Hello" = 2, "World" = 2)) |>
    save_tt("dataframe")
expect_true("Hello" %in% tab[[1]])
expect_true("World" %in% tab[[1]])

# data frame print uses markdown
expect_snapshot_print(tab, "dataframe-print.md")

# Issue #218
options(tinytable_save_output = "dataframe")
tab <- data.frame(x = 1, y = Inf) |>
    tt() |>
    save_tt()
expect_inherits(tab, "data.frame")


# Issue #513: uninformative error
tab <- tt(head(iris)) |>
    group_tt(j = list("a" = 1:2, "b" = 3:5)) |>
    style_tt("colnames", italic = TRUE)
tab <- tab |> save_tt("dataframe")
expect_inherits(tab, "data.frame")
