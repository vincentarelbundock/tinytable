source("helpers.R")

style_resolution_props <- c(
  "bold", "italic", "underline", "strikeout", "monospace", "smallcap",
  "align", "alignv", "color", "background", "fontsize", "indent",
  "html_css", "colspan", "rowspan"
)

make_style_rect <- function() {
  rect <- expand.grid(i = c(0L, 1L, 2L), j = 1:3)
  for (col in style_resolution_props) {
    rect[[col]] <- NA
  }
  rect
}

make_style_settings <- function() {
  data.frame(
    i = c(NA, 1, 2, NA, 1, 2),
    j = c(1, NA, 2, NA, 3, 3),
    bold = c(TRUE, NA, FALSE, NA, TRUE, NA),
    italic = c(NA, TRUE, NA, NA, FALSE, NA),
    underline = NA,
    strikeout = NA,
    monospace = NA,
    smallcap = NA,
    align = c(NA, "l", NA, NA, NA, NA),
    alignv = NA,
    color = c("red", NA, "blue", NA, "green", NA),
    background = c(NA, "yellow", NA, NA, NA, NA),
    fontsize = c(NA, 1.2, NA, NA, 0.9, NA),
    indent = c(NA, NA, 2, NA, NA, NA),
    html_css = c(NA, NA, NA, NA, NA, "font-feature-settings: 'tnum'"),
    colspan = NA,
    rowspan = NA,
    line = c(NA, "b", "t", "b", "l", NA),
    line_color = c(NA, "red", "blue", "gray", "black", NA),
    line_width = c(NA, 0.1, 0.2, 0.4, 0.3, NA),
    line_trim = c(NA, "lr", NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

resolve_styles_reference <- function(rect, settings) {
  other <- rect
  lines <- data.frame()
  for (idx in seq_len(nrow(settings))) {
    row <- settings[idx, , drop = FALSE]
    other <- tinytable:::apply_style_to_rect(other, row)
    lines <- tinytable:::append_lines_to_rect(lines, row, rect)
  }
  list(other = other, lines = lines)
}

rect <- make_style_rect()
settings <- make_style_settings()
reference <- resolve_styles_reference(rect, settings)
resolved <- tinytable:::resolve_styles_batch(rect, settings)

expect_true(isTRUE(all.equal(reference$other, resolved$other, check.attributes = FALSE)))
expect_true(isTRUE(all.equal(reference$lines, resolved$lines, check.attributes = FALSE)))

expected_has_style <- rowSums(!is.na(reference$other[, style_resolution_props, drop = FALSE])) > 0
expect_identical(resolved$has_style, expected_has_style)

expect_true(exists("filter_style_other", envir = asNamespace("tinytable"), inherits = FALSE))
if (exists("filter_style_other", envir = asNamespace("tinytable"), inherits = FALSE)) {
  attr(resolved$other, "has_style") <- resolved$has_style
  compact <- tinytable:::filter_style_other(resolved$other, style_resolution_props)
  expected <- reference$other[expected_has_style, , drop = FALSE]

  expect_true(nrow(compact) < nrow(resolved$other))
  expect_true(isTRUE(all.equal(expected, compact, check.attributes = FALSE)))
}

empty_resolved <- tinytable:::resolve_styles_batch(rect, data.frame())
expect_identical(empty_resolved$has_style, rep(FALSE, nrow(rect)))
attr(empty_resolved$other, "has_style") <- empty_resolved$has_style
expect_identical(
  tinytable:::filter_style_other(empty_resolved$other, style_resolution_props),
  empty_resolved$other[0, , drop = FALSE]
)
