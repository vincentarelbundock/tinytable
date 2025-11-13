source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "latex")

if (!is_local) {
  exit_file("Run on Vincent's machine")
}

x <- mtcars[1:4, 1:5]

expect_snapshot_print(
  tt(x),
  label = "latex-default.tex"
)

k <- x
colnames(k) <- NULL
expect_snapshot_print(
  tt(k),
  label = "latex-nohead.tex"
)

# Issue #613: caption with tabular latex theme
expect_snapshot_print(
  tt(x, caption = "Tabular caption example.") |>
    theme_latex(environment = "tabular"),
  label = "latex-tabular_caption.tex"
)

# Align
expect_snapshot_print(
  tt(x) |> style_tt(j = 1:5, align = "ccllr"),
  label = "latex-align.tex"
)

# Themes
expect_snapshot_print(
  tt(x, theme = "striped"),
  label = "latex-theme_striped.tex"
)

expect_snapshot_print(
  tt(x, theme = "grid"),
  label = "latex-theme_grid.tex"
)

expect_snapshot_print(
  tt(x, theme = "empty"),
  label = "latex-theme_empty.tex"
)

# Styles
expect_snapshot_print(
  tt(x) |> style_tt(i = 1:4, color = "orange"),
  label = "latex-row_color.tex"
)

expect_snapshot_print(
  tt(x) |> style_tt(j = 1:4, color = "orange"),
  label = "latex-col_color.tex"
)

expect_snapshot_print(
  tt(x) |> style_tt(i = 1:2, j = 1:4, color = "orange"),
  label = "latex-cell_color.tex"
)

# Lazy style: group after style is respected
a <- tt(mtcars[1:4, 1:4]) |>
  style_tt(color = "orange", background = "black") |>
  group_tt(j = list("blah" = 1:2, "bar" = 3:4))
b <- tt(mtcars[1:4, 1:4]) |>
  group_tt(j = list("blah" = 1:2, "bar" = 3:4)) |>
  style_tt(color = "orange", background = "black")
expect_snapshot_print(a, label = "latex-group_style_order.tex")
expect_equal(as.character(a@table_string), as.character(b@table_string))

# align d
x <- data.frame(pi = c(pi * 100, pi * 1000, pi * 10000, pi * 100000))
tab <- tt(x) |>
  format_tt(j = 1, digits = 8, num_fmt = "significant_cell") |>
  style_tt(j = 1, align = "d")
expect_snapshot_print(tab, label = "latex-align_d.tex")

dat = data.frame(a = c("(03.1)", "(3.14)**", "(003.1416)+"))
tt(dat) |> style_tt(align = "d")
expect_snapshot_print(tab, label = "latex-align_d_02.tex")

dat = data.frame(a = c("(03.1)", "(3.14)**", "(003.1416)+"))
tt(dat) |>
  group_tt(j = list("blah" = 1)) |>
  style_tt(align = "d")
expect_snapshot_print(tab, label = "latex-align_d_03.tex")

# bug discovered with vignette
x <- tt(mtcars[1:9, 1:8]) |>
  group_tt(
    i = list(
      "I like (fake) hamburgers" = 3,
      "She prefers halloumi" = 4,
      "They love tofu" = 7
    )
  ) |>
  style_tt(
    i = c(3, 5, 9),
    align = "c",
    color = "white",
    background = "gray",
    bold = TRUE
  ) |>
  save_tt("latex")
expect_inherits(x, "character")

# Footnotes
expect_snapshot_print(
  (tt(mtcars[1:4, 1:5], notes = list(a = "Blah.", b = "Blah blah."))),
  label = "latex-footnotes.tex"
)

# Maths
math <- tt(
  data.frame(
    Math = c(
      "$x^2 + y^2 = z^2$",
      "$\\frac{1}{2}$"
    )
  )
) |>
  style_tt(j = 1, align = "c")
expect_snapshot_print(
  math,
  label = "latex-maths.tex"
)

# Line breaks
expect_snapshot_print(
  (tt(
    data.frame(
      "{Sed ut \\\\ perspiciatis unde}",
      "dicta sunt<br> explicabo. Nemo"
    ) |>
      setNames(c("LaTeX line break", "HTML line break")),
    width = 1
  )),
  label = "latex-breaks.tex"
)

# Formatting
dat <- data.frame(
  a = c("Burger", "Halloumi", "Tofu", "Beans"),
  b = c(1.43202, 201.399, 0.146188, 0.0031),
  c = c(98938272783457, 7288839482, 29111727, 93945)
)
expect_snapshot_print(
  (tt(dat) |>
    format_tt(j = "a", sprintf = "Food: %s") |>
    format_tt(j = 2, digits = 1) |>
    format_tt(j = "c", digits = 2, num_suffix = TRUE)),
  label = "latex-formatting.tex"
)

# placement
tab <- mtcars[1:3, 1:3]
tab <- tt(tab) |> theme_latex(placement = "H")
tab@output <- "latex"
expect_snapshot_print(tab, label = "latex-placement.tex")

# Missing value replacement
tab <- data.frame(a = c(NA, 1, 2), b = c(3, NA, 5))
expect_snapshot_print(
  tt(tab) |> format_tt(replace = "-"),
  label = "latex-missing_value_replacement.tex"
)

# Escape special characters
dat <- data.frame(
  "LaTeX" = c("Dollars $", "Percent %", "Underscore _"),
  "HTML" = c("<br>", "<sup>4</sup>", "<emph>blah</emph>")
)
expect_snapshot_print(
  tt(dat) |> format_tt(escape = TRUE),
  label = "latex-escape_special_caracters.tex"
)

# Formatting URLs
dat <- data.frame(
  `Package (link)` = c(
    "[`marginaleffects`](https://www.marginaleffects.com/)",
    "[`modelsummary`](https://www.modelsummary.com/)"
  ),
  Purpose = c(
    "Interpreting statistical models",
    "Data and model summaries"
  ),
  check.names = FALSE
)
dat <- tt(dat) |> format_tt(j = 1, markdown = TRUE)
expect_snapshot_print(
  dat,
  label = "latex-formatting_url.tex"
)

# Style
x <- mtcars[1:4, 1:5]
expect_snapshot_print(
  (tt(x) |>
    style_tt(
      i = 2:3,
      j = c(1, 3, 4),
      italic = TRUE,
      background = "green",
      color = "orange"
    )),
  label = "latex-style.tex"
)

# Font size
expect_snapshot_print(
  (tt(x) |> style_tt(j = "mpg|hp|qsec", fontsize = 1.5)),
  label = "latex-font_size.tex"
)

# Merging cells
expect_snapshot_print(
  (tt(x) |>
    style_tt(
      i = 2,
      j = 2,
      colspan = 3,
      rowspan = 2,
      align = "c",
      alignv = "m",
      color = "white",
      background = "black",
      bold = TRUE
    )),
  label = "latex-merging_cells.tex"
)

# Spanning cells
tab <- aggregate(mpg ~ cyl + am, FUN = mean, data = mtcars)
tab <- tab[order(tab$cyl, tab$am), ]
expect_snapshot_print(
  (tt(tab, digits = 2) |>
    style_tt(i = c(1, 3, 5), j = 1, rowspan = 2, alignv = "t")),
  label = "latex-spanning_cells.tex"
)

# Conditional styling
k <- mtcars[1:10, c("mpg", "am", "vs")]
expect_snapshot_print(
  (tt(k) |>
    style_tt(
      i = which(k$am == k$vs),
      background = "teal",
      color = "white"
    )),
  label = "latex-conditional_styling.tex"
)

# Heatmaps
fs <- seq(.1, 2, length.out = 20)
k <- data.frame(matrix(fs, ncol = 5))
colnames(k) <- NULL
bg <- hcl.colors(20, "Inferno")
fg <- ifelse(as.matrix(k) < 1.7, tail(bg, 1), head(bg, 1))
expect_snapshot_print(
  (tt(k, width = .7, theme = "empty") |>
    style_tt(j = 1:5, align = "ccccc") |>
    style_tt(
      i = 1:4,
      j = 1:5,
      color = fg,
      background = bg,
      fontsize = fs
    )),
  label = "latex-heatmaps.tex"
)

# NA values in background styling
if (Sys.info()["sysname"] == "Darwin") {
  set.seed(48103)
  bg <- sample(c(NA, "green", "orange"), 6 * 5, replace = TRUE)
  expect_snapshot_print(
    tt(head(iris)) |>
      style_tt(i = 1:6, j = 1:5, background = bg),
    label = "latex-background_na_values.tex"
  )
}

# Borders
expect_snapshot_print(
  tt(x, theme = "empty") |>
    style_tt(
      i = 0:3,
      j = 1:3,
      line = "tblr",
      line_width = 0.4,
      line_color = "orange"
    ),
  label = "latex-borders.tex"
)

# Issue #242: multiple notes in tabularray are fiddly
tab <- tt(
  mtcars[1:4, 1:4],
  notes = list(a = "blah", b = "hello world", "oh yeah", "works?")
)
tab@output <- "latex"
expect_snapshot_print(tab, label = "latex-issue242.tex")

# Issue #306
x <- data.frame(x = 1:5)
x <- data.frame(x = 1:5)
colnames(x) <- NULL
tab <- tt(x) |>
  format_tt() |>
  save_tt("latex")
expect_inherits(tab, "character")

# Issue #307
tab <- tt(head(iris)) |>
  group_tt(j = list("blah" = 1:2)) |>
  theme_latex(environment = "tabular", environment_table = FALSE)
expect_snapshot_print(tab, label = "latex-issue307.tex")

# Issue #419
tab <- tt(head(iris)) |>
  theme_latex(
    outer = "label={tbl-species}",
    inner = "rowsep=0pt"
  )
expect_snapshot_print(tab, label = "latex-issue419.tex")

# format_tt math
dat <- data.frame("y^2 = e^x" = c(-2, -pi), check.names = FALSE)
tab <- tt(dat, digits = 3) |> format_tt(math = TRUE)
expect_snapshot_print(tab, label = "latex-format_tt_math.tex")


# Issue #493: theme_multipage does not convert to longtblr
dat <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
tab <- tt(dat) |>
  theme_latex(multipage = TRUE, rowhead = 1) |>
  save_tt("latex")
matches <- lengths(gregexpr("longtblr", tab, fixed = TRUE))
expect_equal(matches, 2)


# Issue #535: environment_table = FALSE without an environment argument
tab1 <- head(mtcars, 1) |>
  tt() |>
  theme_latex(environment_table = FALSE) |>
  save_tt("latex")
table_matches <- gregexpr("\\\\begin\\{table\\}", tab1)[[1]]
expect_equal(ifelse(table_matches[1] == -1, 0, length(table_matches)), 0)
expect_equal(lengths(gregexpr("\\{tblr", tab1)), 2)

tab2 <- head(mtcars, 1) |>
  tt() |>
  theme_latex(environment = "longtblr") |>
  save_tt("latex")
table_matches2 <- gregexpr("\\\\begin\\{table\\}", tab2)[[1]]
expect_equal(ifelse(table_matches2[1] == -1, 0, length(table_matches2)), 0)
expect_equal(lengths(gregexpr("longtblr", tab2)), 2)

tab3 <- head(mtcars, 1) |>
  tt() |>
  theme_latex(environment = "talltblr") |>
  save_tt("latex")
expect_equal(lengths(gregexpr("\\{table", tab3)), 2)
expect_equal(lengths(gregexpr("\\{talltblr", tab3)), 2)



options(tinytable_print_output = NULL)
