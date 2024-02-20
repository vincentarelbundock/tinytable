source("helpers.R")
using("tinysnapshot")


# numeric vector input
a <- c(98938272783457, 7288839482, 29111727, 93945)
b <- format_tt(a, num_suffix = TRUE, digits = 1)
expect_equivalent(b, c("98.9T", "7.3B", "29.1M", "93.9K"))


# num_suffix (vignette)
options(tinytable_print_output = "markdown")
dat <- data.frame(
     a = c("Burger", "Halloumi", "Tofu", "Beans"),
     b = c(1.43202, 201.399, 0.146188, 0.0031),
     c = c(98938272783457, 7288839482, 29111727, 93945))
tab <- tt(dat) |>
  format_tt(j = "a", sprintf = "Food: %s") |>
  format_tt(j = 2, digits = 1) |>
  format_tt(j = "c", digits = 2, num_suffix = TRUE)
expect_snapshot_print(tab, label = "format_tt-num_suffix_vignette")
options(tinytable_print_output = "markdown")
