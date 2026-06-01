source("helpers.R")
using("tinysnapshot")
requiet("data.table")

dat1 <- structure(list(EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")), class = "data.frame", row.names = c(NA, -27L)) # noqa
DT1 <- as.data.table(dat1)
colnames(DT1) <- NULL
tab <- tt(DT1)
expect_inherits(tab, "tinytable")


# Bug group_tt(i) accepts factor variables
dat <- do.call(rbind, by(iris, iris$Species, head, n = 3))
tab <- tt(dat) |>
  group_tt(dat$Species) |>
  save_tt("markdown")
tab <- unlist(strsplit(tab, "\\n"))
expect_equal(sum(grepl("^\\| virginica", tab)), 1)


# Bug: OutDec should not affect structural markup numbers
local({
  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)

  has_bad_markup_decimal <- function(x) {
    grepl("[0-9],[0-9]+(em|%|deg)", x, perl = TRUE)
  }

  dat <- data.frame(a = 1.23, b = 4.56)
  tab <- tt(dat, height = 0.25, width = 0.333) |>
    format_tt(digits = 2, num_fmt = "decimal") |>
    style_tt(
      i = 1,
      j = 1,
      fontsize = 0.8,
      indent = 0.5,
      line = "tblr",
      line_width = 0.05
    ) |>
    theme_rotate(angle = 12.5, i = 1, j = 1)

  html <- save_tt(tab, "html")
  latex <- save_tt(tab, "latex")
  typst <- save_tt(tab, "typst")
  tabulator <- tt(dat, height = 0.25) |>
    format_tt(digits = 2, num_fmt = "decimal") |>
    theme_html(engine = "tabulator") |>
    save_tt("html")

  expect_false(has_bad_markup_decimal(html))
  expect_false(has_bad_markup_decimal(latex))
  expect_false(has_bad_markup_decimal(typst))
  expect_false(has_bad_markup_decimal(tabulator))

  expect_true(grepl("1,23", html, fixed = TRUE))
  expect_true(grepl("1,23", latex, fixed = TRUE))
  expect_true(grepl("1,23", typst, fixed = TRUE))
  expect_true(grepl("1,23", tabulator, fixed = TRUE))
})
