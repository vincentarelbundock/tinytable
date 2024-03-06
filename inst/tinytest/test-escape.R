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
