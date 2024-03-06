source("helpers.R")
using("tinysnapshot")

dat <- data.frame(
    "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
    "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
)

set.seed(1024) # reproducibility of html unique IDs
tab <- tt(dat) |> format_tt(escape = TRUE) |> save_tt("latex")
expect_snapshot_print(tab, "escape-latex")

# escape columns
dat <- data.frame("blah_blah" = 1:2)
tab <- tt(dat) |> format_tt(escape = TRUE) |> save_tt("latex")
expect_snapshot_print(tab, "escape-latex_colnames")


# Issue #150: avoid double escaping captions
tab <- tt(head(iris), caption = "Blah blah \\label{tab:blah-blah}")
tab@output <- "latex"
expect_snapshot_print(tab, "escape-issue150_caption_escape")


tab <- mtcars[1:3, 1:4] |>
    setNames(c("blah_blah", "foo_bar", "c", "d")) |>
    tt() |>
    format_tt(escape = TRUE) |>
    group_tt(j = list("foo_bar" = 1:2, "banana_fish" = 3:4))
tab@output <- "latex"
expect_snapshot_print(tab, "escape-issue150_caption_escape_02")


# Escape caption and notes
tab <- mtcars[1:3, 1:4] |>
    setNames(c("blah_blah", "foo_bar", "c", "d")) |>
    tt(caption = "banana_fish", notes = list("banana_fish", "b" = list(i = 1, j = 1:2, text = "banana_fish"))) |>
    format_tt(escape = TRUE) |>
    group_tt(j = list("foo_bar" = 1:2, "banana_fish" = 3:4))
tab@output <- "latex"
expect_snapshot_print(tab, "escape-issue150_caption_escape_03")

