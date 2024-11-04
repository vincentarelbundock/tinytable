source("helpers.R")
using("tinysnapshot")


# vector row labels
set.seed(48103)
dat <- data.frame(
    label = c("a", "a", "a", "b", "b", "c", "a", "a"),
    x1 = rnorm(8),
    x2 = rnorm(8))
tab <- tt(dat[, 2:3]) |> group_tt(i = dat$label)
expect_snapshot_print(tab, label = "group_tt-vector_row_labels.md")

# 3 level: markdown
options(tinytable_print_output = "markdown")
x <- mtcars[1:3, 1:5]
tab <- tt(x) |>
    group_tt(j = list("a" = 2:3, "b" = 4:5)) |>
    group_tt(j = list("c" = 1:2, "d" = 3:5)) |>
    group_tt(j = list("e" = 1:3, "f" = 4))
expect_snapshot_print(tab, label = "group_tt-3level.md")
options(tinytable_print_output = NULL)


# 3 level: latex
options(tinytable_print_output = "latex")
x <- mtcars[1:3, 1:5]
tab <- tt(x) |>
    group_tt(j = list("a" = 2:3, "b" = 4:5)) |>
    group_tt(j = list("c" = 1:2, "d" = 3:5)) |>
    group_tt(j = list("e" = 1:3, "f" = 4))
expect_snapshot_print(tab, label = "group_tt-3level.tex")
options(tinytable_print_output = NULL)


# 3 level: typst
x <- mtcars[1:3, 1:5]
tab <- tt(x) |>
    group_tt(j = list("a" = 2:3, "b" = 4:5)) |>
    group_tt(j = list("c" = 1:2, "d" = 3:5)) |>
    group_tt(j = list("e" = 1:3, "f" = 4))
tab@output <- "typst"
expect_snapshot_print(tab, label = "group_tt-3level.typ")



tab <- tt(mtcars[1:10, 1:5]) |>
    group_tt(
        i = list(
            "Hello" = 3,
            "World" = 8),
        j = list(
            "Foo" = 2:3,
            "Bar" = 4:5))
expect_snapshot_print(print_html(tab), "group_tt-html_tutorial_01.html")





# Issue #165: Column labels not centered, last row not styled because of row group offset
k = mtcars[1:5, 1:5]
k[1, 1] <- pi
tab <- tt(k) |>
    format_tt(num_fmt = "decimal", num_zero = TRUE, digits = 3) |>
    style_tt(i = 1:3, j = 1, bold = TRUE) |>
    style_tt(j = 2, italic = TRUE) |>
    group_tt(i = list("hello world" = 2), j = list("foo" = 1:2, "bar" = 3:4))
expect_snapshot_print(print_html(tab), "group_tt-issue165_html_centering_style.html")


# Issue #165: group_tt insert extra row at the bottom
k <- mtcars[1:4, 1:4]
tab <- tt(k) |>
    format_tt(num_fmt = "decimal", num_zero = TRUE, digits = 3) |>
    style_tt(i = 1:3, j = 1, bold = TRUE) |>
    style_tt(j = 2, italic = TRUE) |>
    group_tt(i = list("hello world" = 2), j = list("foo" = 1:2, "bar" = 3:4), italic = TRUE)
expect_snapshot_print(tab, "group_tt-issue165_extra_row")


# Issue #258: group_tt indices with 0 and duplicates
for (o in c("latex", "typst", "markdown", "html")) {
    options(tinytable_print_output = o)

    lab = sprintf("group_tt-issue258_01_%s", o)
    tab <- tt(head(iris)) |> group_tt(i = list("hello" = 1))
    if (o == "html") {
        lab <- paste0(lab, ".html")
        expect_snapshot_print(print_html(tab), lab)
    } else {
        expect_snapshot_print(tab, lab)
    }

    lab = sprintf("group_tt-issue258_02_%s", o)
    tab <- tt(head(iris)) |> group_tt(i = list("hello" = 2, "world" = 2))
    if (o == "html") {
        lab <- paste0(lab, ".html")
        expect_snapshot_print(print_html(tab), lab)
    } else {
        expect_snapshot_print(tab, lab)
    }

    options(tinytable_print_output = NULL)
}
