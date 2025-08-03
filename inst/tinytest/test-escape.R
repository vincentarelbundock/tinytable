source("helpers.R")
using("tinysnapshot")

dat <- data.frame(
  "LaTeX_Special" = c(
    "Dollars $",
    "Percent %",
    "Underscore _",
    "Backslash \\",
    "Hash #",
    "Ampersand &"
  ),
  "HTML_Special" = c(
    "<br>",
    "<sup>4</sup>",
    "<emph>blah</emph>",
    "&amp;",
    "&lt;",
    "&gt;"
  ),
  "Mixed_Content" = c(
    "$100 & <b>bold</b>",
    "50% & <i>italic</i>",
    "user_name & <code>code</code>"
  )
)

set.seed(1024) # reproducibility of html unique IDs
tab <- tt(dat) |> format_tt(escape = TRUE)
t <- expect_table(tab, formats = c("html"))
expect_snapshot_print(t[["html"]], "escape-latex.html")
tab <- tt(dat, escape = TRUE)
t <- expect_table(tab, formats = c("html"))
expect_snapshot_print(t[["html"]], "escape-latex.html")

tab <- tt(dat, escape = TRUE)
t <- expect_table(tab, formats = c("all", "html", "typst"))
expect_snapshot_print(t[["latex"]], "escape-simple.tex")
expect_snapshot_print(t[["html"]], "escape-simple.html")
expect_snapshot_print(t[["typst"]], "escape-simple.typ")

# escape columns
dat <- data.frame(
  "blah_blah_underscore" = 1:2,
  "dollar$sign" = 3:4,
  "percent%sign" = 5:6
)
tab <- tt(dat) |> format_tt(escape = TRUE)
t <- expect_table(tab, formats = c("latex", "html", "typst"))
expect_snapshot_print(t[["latex"]], "escape-latex_colnames.tex")
expect_snapshot_print(t[["html"]], "escape-latex_colnames.html")
expect_snapshot_print(t[["typst"]], "escape-latex_colnames.typ")

# Issue #150: avoid double escaping captions
tab <- tt(
  head(iris),
  caption = "Blah blah $100 & <b>bold</b> \\label{tab:blah-blah}"
)
t <- expect_table(tab, formats = c("latex", "html", "typst"))
expect_snapshot_print(t[["latex"]], "escape-issue150_caption.tex")
expect_snapshot_print(t[["html"]], "escape-issue150_caption.html")
# backslash should be escpaed in typst, so this breaks compile
# expect_snapshot_print(t[["typst"]], "escape-issue150_caption.typ")

tab <- mtcars[1:3, 1:4] |>
  setNames(c(
    "blah_blah_underscore",
    "dollar$sign",
    "percent%sign",
    "ampersand&sign"
  )) |>
  tt() |>
  format_tt(escape = TRUE) |>
  group_tt(j = list("foo_bar" = 1:2, "banana_fish" = 3:4))
t <- expect_table(tab, formats = c("latex", "html", "typst"))
expect_snapshot_print(t[["latex"]], "escape-issue150_caption_02.tex")
expect_snapshot_print(t[["html"]], "escape-issue150_caption_02.html")
expect_snapshot_print(t[["typst"]], "escape-issue150_caption_02.typ")

# Escape caption and notes
tab <- mtcars[1:3, 1:4] |>
  setNames(c(
    "blah_blah_underscore",
    "dollar$sign",
    "percent%sign",
    "ampersand&sign"
  )) |>
  tt(
    caption = "banana_fish $100 & <b>bold</b>",
    notes = list(
      "banana_fish $100 & <b>bold</b>",
      "b" = list(i = 1, j = 1:2, text = "banana_fish $100 & <b>bold</b>")
    )
  ) |>
  format_tt(escape = TRUE) |>
  group_tt(j = list("foo_bar" = 1:2, "banana_fish" = 3:4))
t <- expect_table(tab, formats = c("latex", "html", "typst"))
expect_snapshot_print(t[["latex"]], "escape-issue150_caption_03.tex")
expect_snapshot_print(t[["html"]], "escape-issue150_caption_03.html")
expect_snapshot_print(t[["typst"]], "escape-issue150_caption_03.typ")
