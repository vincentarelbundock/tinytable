source("helpers.R")
using("tinysnapshot")

dat <- data.frame(
    "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
    "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
)

set.seed(1024) # reproducibility of html unique IDs
tab <- tt(dat) |> format_tt(escape = TRUE) |> save_tt("latex")
expect_snapshot_print(tab, "escape-latex")

tab <- tt(dat) |> format_tt(escape = TRUE) |> save_tt("html")
expect_snapshot_print(tab, "escape-html")

# escape columns
dat <- data.frame("blah_blah" = 1:2)
tab <- tt(dat) |> format_tt(escape = TRUE) |> save_tt("latex")
expect_snapshot_print(tab, "escape-latex_colnames")
