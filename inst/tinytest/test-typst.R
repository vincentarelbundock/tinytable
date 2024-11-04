source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "typst")



# semi complicated
tab <- tt(mtcars[1:4, 1:5], caption = "Hello World") |>
  group_tt(j = list("Group 1" = 4:5, "Group 2" = 2:3)) |>
  style_tt(j = 1:5, align = "lcccr") |>
  style_tt(i = 2, j = 1:3, strikeout = TRUE, bold = TRUE, background = "black", color = "white") |>
  style_tt(j = 1, color = "red", italic = TRUE)
expect_snapshot_print(tab, label = "typst-complicated")


# issue #139
tab <- tt(mtcars[1:10, 1:5]) |>
  group_tt(i = list("Feta" = 2, "Brie" = 6)) |>
  group_tt(j = list("First" = 2:3, "Second" = 4:5)) |>
  style_tt(1:5, align = "c", background = "blue", color = "white")
expect_snapshot_print(tab, label = "typst-issue-139_misaligned_rule_with_group_tt")

# Theme striped
x <- mtcars[1:4, 1:5]
tab <- tt(x, theme = "striped")
expect_snapshot_print(tab, label = "typst-striped")

# Theme grid
tab <- tt(x, theme = "grid")
expect_snapshot_print(tab, label = "typst-grid")

# Formatting
dat <- data.frame(
  w = c(143002.2092, 201399.181, 100188.3883),
  x = c(1.43402, 201.399, 0.134588),
  y = as.Date(c(897, 232, 198), origin = "1970-01-01"),
  z = c(TRUE, TRUE, FALSE))
dat <- tt(dat, digits = 2)
expect_snapshot_print(dat, label = "typst-formatting")

# More formatting
dat <- data.frame(
  a = c("Burger", "Halloumi", "Tofu", "Beans"),
  b = c(1.43202, 201.399, 0.146188, 0.0031),
  c = c(98938272783457, 7288839482, 29111727, 93945))
tab <- tt(dat) |>
  format_tt(j = "a", sprintf = "Food: %s") |>
  format_tt(j = 2, digits = 1) |>
  format_tt(j = "c", digits = 2, num_suffix = TRUE)
expect_snapshot_print(tab, label = "typst-more_formatting")

# Significant cell
dat <- data.frame(x = c(0.000123456789, 12.4356789))
tab <- tt(dat) |> format_tt(digits = 2, num_fmt = "significant_cell")
expect_snapshot_print(tab, label = "typst-significant_cell")

# Missing value replacement
tab <- tt(data.frame(a = c(NA, 1, 2), b = c(3, NA, 5)))
tab <- format_tt(tab, replace = "-")
expect_snapshot_print(tab, label = "typst-missing_value_replacement")

# Italic markdown
dat <- data.frame(markdown = c("This is _italic_ text."))
tab <- tt(dat) |>
  format_tt(j = 1, markdown = TRUE) |>
  style_tt(j = 1, align = "c")
expect_snapshot_print(tab, label = "typst-italic_markdown")

# Font size
dat <- tt(x) |> style_tt(j = "mpg|hp|qsec", fontsize = 1.5)
expect_snapshot_print(dat, label = "typst-font_size")

# No headers
k <- x
colnames(k) <- NULL
k <- tt(k)
expect_snapshot_print(k, label = "typst-no_headers")

# Group rows
dat <- mtcars[1:9, 1:8]
dat <- tt(dat) |>
  group_tt(i = list(
    "I like (fake) hamburgers" = 3,
    "She prefers halloumi" = 4,
    "They love tofu" = 7))
expect_snapshot_print(dat, label = "typst-group_rows")

# Group columns
dat <- mtcars[1:9, 1:8]
tab <- tt(dat) |>
  group_tt(
    j = list(
      "Hamburgers" = 1:3,
      "Halloumi" = 4:5,
      "Tofu" = 7))
expect_snapshot_print(dat, label = "typst-group_columns")


# issue #323
dat <- mtcars[1:9, 1:8]
tab <- tt(dat) |> 
  group_tt(
    i = list("I like (fake) hamburgers" = 3,
             "She prefers halloumi" = 4,
             "They love tofu" = 7),
    j = list("Hamburgers" = 1:3,
             "Halloumi" = 4:5,
             "Tofu" = 7)) |>
  style_tt(
    i = c(3, 5, 9),
    align = "c",
    background = "black",
    color = "orange") |>
  style_tt(i = -1, color = "orange")
tab@output <- "typst"
expect_snapshot_print(tab, label = "typst-issue323_group_tt_style_tt")


# Frame
tab <- tt(mtcars[1:5, 1:5]) |>
    style_tt(2:3, 2:3, line_color = "red", line = "tblr", line_width = .05)
tab@output <- "typst"
expect_snapshot_print(tab, label = "typst-tblr")


# Issue #357
tab <- tt(head(iris), notes = "blah") |> save_tt("typst")
expect_true(grepl('blah', tab))


options(tinytable_print_output = NULL)


