source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "latex")

dat <- data.frame(
  "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
  "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
)

set.seed(1024) # reproducibility of html unique IDs
tab <- tt(dat) |>
  format_tt(escape = TRUE)
expect_snapshot_print(tab, "escape-latex.tex")

tab <- tt(dat, escape = TRUE)
expect_snapshot_print(tab, "escape-latex.tex")

options(tinytable_tt_escape = TRUE)
tab <- tt(dat)
expect_snapshot_print(tab, "escape-latex.tex")
options(tinytable_tt_escape = FALSE)

# escape columns
dat <- data.frame("blah_blah" = 1:2)
tab <- tt(dat) |> format_tt(escape = TRUE)
expect_snapshot_print(tab, "escape-latex_colnames.tex")

# Issue #150: avoid double escaping captions
tab <- tt(head(iris), caption = "Blah blah \\label{tab:blah-blah}")
expect_snapshot_print(tab, "escape-issue150_caption_escape.tex")

tab <- mtcars[1:3, 1:4] |>
  setNames(c("blah_blah", "foo_bar", "c", "d")) |>
  tt() |>
  format_tt(escape = TRUE) |>
  group_tt(j = list("foo_bar" = 1:2, "banana_fish" = 3:4))
expect_snapshot_print(tab, "escape-issue150_caption_escape_02.tex")

# Escape caption and notes
tab <- mtcars[1:3, 1:4] |>
  setNames(c("blah_blah", "foo_bar", "c", "d")) |>
  tt(
    caption = "banana_fish",
    notes = list(
      "banana_fish",
      "b" = list(i = 1, j = 1:2, text = "banana_fish")
    )
  ) |>
  format_tt(escape = TRUE) |>
  group_tt(j = list("foo_bar" = 1:2, "banana_fish" = 3:4))
expect_snapshot_print(tab, "escape-issue150_caption_escape_03.tex")
