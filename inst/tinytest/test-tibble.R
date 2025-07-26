source("helpers.R")
using("tinysnapshot")
requiet("tibble")
requiet("pillar")
exit_file("fail on github actions")

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
expect_snapshot_print(tab, "tibble-tibble_issue141")


options("tinytable_print_output" = NULL)
x <- tibble(
  x3 = pillar::num(9:11 * 100 + 0.5, sigfig = 3),
  x4 = pillar::num(9:11 * 100 + 0.5, sigfig = 4),
  x5 = pillar::num(9:11 * 100 + 0.5, sigfig = 5),
)
tab <- tt(x)
t <- expect_table(tab)
expect_snapshot_print(t[["typst"]], "tibble-pillar_num.typ")
expect_snapshot_print(t[["markdown"]], "tibble-pillar_num.md")
expect_snapshot_print(t[["latex"]], "tibble-pillar_num.tex")

exit_file("fail on github actions")
expect_snapshot_print(t[["html"]], "tibble-pillar_num.html")


# markdown
exit_file("ANSI tinysnapshot issue")
options("tinytable_print_output" = "markdown")
expect_snapshot_print(tab, "tibble-pillar_num")
options("tinytable_print_output" = NULL)
