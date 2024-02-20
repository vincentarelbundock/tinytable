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

set.seed(1024)
dat <- data.frame(
     w = c(143002.2092, 201399.181, 100188.3883),
     x = c(1.43402, 201.399, 0.134588),
     y = as.Date(sample(1:1000, 3)),
     z = c(TRUE, TRUE, FALSE))
tab <- tt(dat, digits = 2)
expect_snapshot_print(tab, label = "format_tt-vignette_digits")

tab <- tt(dat) |> 
  format_tt(
    j = 2:4,
    digits = 1,
    date = "%B %d %Y") |>
  format_tt(
    j = 1,
    digits = 2,
    num_mark_big = " ",
    num_mark_dec = ",",
    num_fmt = "decimal")
expect_snapshot_print(tab, label = "format_tt-vignette_misc")

options(tinytable_print_output = "markdown")
