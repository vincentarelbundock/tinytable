source("helpers.R")
using("tinysnapshot")

options(tinytable_print_output = "markdown")
x <- mtcars[1:4, 1:6]
tab <- tt(x) |>
  format_tt(j = "drat|wt", num_fmt = "decimal", digits = 4, num_zero = TRUE)
expect_snapshot_print(tab, "i_j-sanitize_j_equivalence")

tab <- tt(x) |>
  format_tt(j = c("drat", "wt"), num_fmt = "decimal", num_zero = TRUE, digits = 4)
expect_snapshot_print(tab, "i_j-sanitize_j_equivalence")

tab <- tt(x) |>
  format_tt(j = c(5:6), num_fmt = "decimal", num_zero = TRUE, digits = 4)
expect_snapshot_print(tab, "i_j-sanitize_j_equivalence")

tab <- tt(x) |>
  format_tt(i = 1:2, j = c(5:6), num_fmt = "decimal", digits = 4) |>
  format_tt(i = 3:4, j = c(5:6), num_fmt = "decimal", digits = 2)
expect_snapshot_print(tab, "i_j-format_tt_i_01")

cormat <- data.frame(cor(mtcars[1:5]))
tab <- tt(cormat, digits = 2) |>
  style_tt(i = abs(cormat) > .8, background = "black", color = "white")
expect_equal(dim(tab), c(5, 5))


expect_error(tt(head(iris)) |> style_tt(j = "BadColumn") |> print(),
  pattern = "No columns match the pattern")

options(tinytable_print_output = NULL)
