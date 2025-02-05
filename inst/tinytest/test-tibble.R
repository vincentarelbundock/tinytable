source("helpers.R")
using("tinysnapshot")
requiet("tibble")
requiet("pillar")


# Issue #426
x <- tibble(a = c(pi, 2.3, 1), b = letters[1:3])
x <- format_tt(x, digits = 1)
expect_inherits(x, "tbl_df")
expect_equivalent(x[[1]], c("3", "2", "1"))
expect_equivalent(x[[2]], c("a", "b", "c"))


# Issue #141
k2 <- tibble(
  x3 = 9:11 * 100 + 0.5,
  x4 = 9:11 * 100 + 0.5,
  x5 = 9:11 * 100 + 0.5,
) 
tab <- tt(k2, digits = 1)
options("tinytable_print_output" = "markdown")
expect_snapshot_print(tab, "tibble-markdown_tibble_issue141")


x <- tibble(
  x3 = pillar::num(9:11 * 100 + 0.5, sigfig = 3),
  x4 = pillar::num(9:11 * 100 + 0.5, sigfig = 4),
  x5 = pillar::num(9:11 * 100 + 0.5, sigfig = 5),
) 

tab <- tt(x)

options("tinytable_print_output" = "typst")
expect_snapshot_print(tab, "tibble-typst_pillar_num")

options("tinytable_print_output" = "latex")
expect_snapshot_print(tab, "tibble-latex_pillar_num")

options("tinytable_print_output" = NULL)



exit_file("ANSI tinysnapshot issue")
options("tinytable_print_output" = "markdown")
expect_snapshot_print(tab, "tibble-markdown_pillar_num")
options("tinytable_print_output" = NULL)




exit_file("works interactively")
options("tinytable_print_output" = "html")
expect_snapshot_print(print_html(tab), "tibble-html_pillar_num.html")
options("tinytable_print_output" = NULL)
