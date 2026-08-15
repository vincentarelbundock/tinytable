source("helpers.R")
using("tinysnapshot")

dat <- head(iris[, 1:3], 3)

# Render to a single string so grepl() checks below are scalar
render <- function(x, format) {
  paste(build_tt(x, output = format)@table_string, collapse = "\n")
}

# Validation: only the three portable types are accepted
expect_error(
  render(style_tt(tt(dat), i = 1, line = "b", line_type = "dotdash"), "html"),
  pattern = "solid, dashed, dotted"
)

# Default is unchanged: no line_type means solid everywhere
tab <- tt(dat) |> style_tt(i = 1, line = "b")
expect_true(grepl("--line-type-bottom: solid", render(tab, "html"), fixed = TRUE))
expect_true(grepl("solid, black", render(tab, "latex"), fixed = TRUE))
expect_true(grepl("stroke: 0.1em + black", render(tab, "typst"), fixed = TRUE))

# HTML: the type reaches the per-cell custom property
tab <- tt(dat) |> style_tt(i = 1, line = "b", line_type = "dashed")
expect_true(grepl("--line-type-bottom: dashed", render(tab, "html"), fixed = TRUE))

# LaTeX: tabularray hline spec
expect_true(grepl("hline{3}={1-3}{dashed, black", render(tab, "latex"), fixed = TRUE))

# Typst: dictionary stroke form, only used for non-solid rules
expect_true(grepl('dash: "dashed"', render(tab, "typst"), fixed = TRUE))

# Dotted
tab <- tt(dat) |> style_tt(i = 1, line = "b", line_type = "dotted")
expect_true(grepl("--line-type-bottom: dotted", render(tab, "html"), fixed = TRUE))
expect_true(grepl("{dotted, black", render(tab, "latex"), fixed = TRUE))
expect_true(grepl('dash: "dotted"', render(tab, "typst"), fixed = TRUE))

# Compound line specs apply the type to every requested side
tab <- tt(dat) |> style_tt(i = 2, j = 2, line = "tblr", line_type = "dashed")
h <- render(tab, "html")
for (side in c("top", "bottom", "left", "right")) {
  expect_true(grepl(sprintf("--line-type-%s: dashed", side), h, fixed = TRUE))
}

# Vertical rules in Typst also carry the dash
expect_true(grepl('table\\.vline\\(.*dash: "dashed"', render(tab, "typst")))

# Types are not merged across rules: distinct rules keep distinct types
tab <- tt(dat) |>
  style_tt(i = 1, line = "b", line_type = "dashed") |>
  style_tt(i = 2, line = "b", line_type = "dotted")
h <- render(tab, "html")
expect_true(grepl("--line-type-bottom: dashed", h, fixed = TRUE))
expect_true(grepl("--line-type-bottom: dotted", h, fixed = TRUE))

# HTML draws borders on pseudo-elements. The stylesheet is often loaded from a
# CDN, where an older copy always draws solid rules, so the type-aware rules
# travel with the table itself -- but only when a non-solid type is used.
expect_true(grepl("var(--line-type-bottom, solid)", h, fixed = TRUE))
expect_false(grepl(
  "var(--line-type-bottom, solid)",
  render(tt(dat) |> style_tt(i = 1, line = "b"), "html"),
  fixed = TRUE
))

# Markdown ignores line types rather than erroring
expect_inherits(render(tab, "markdown"), "character")

# Snapshots
tab <- tt(dat) |>
  style_tt(i = 1, line = "b", line_type = "dashed") |>
  style_tt(i = 2, line = "b", line_type = "dotted", line_color = "red") |>
  style_tt(i = 3, j = 2, line = "tblr", line_type = "dashed")
t <- expect_table(tab, c("html", "latex", "typst"))
expect_snapshot_print(t[["html"]], "line_type-mixed.html")
expect_snapshot_print(t[["latex"]], "line_type-mixed.tex")
expect_snapshot_print(t[["typst"]], "line_type-mixed.typ")
