
source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "html")



x <- mtcars[1:4, 1:5]

tab <- tt(x, theme = "striped")
expect_snapshot_print(print_html(tab), "html-striped")

tab <- tt(x, theme = "striped") |>
  style_tt(color = "orange")
expect_snapshot_print(print_html(tab), "html-striped_orange")


# Issue #92: header alignment
k <- structure(list(Column1 = c("Some text", "123"), Column2 = c("Some text", 
"456")), row.names = c(NA, -2L), class = "data.frame")
tab <- tt(k, width = 1) |> style_tt(j = 2, align = "r")
expect_snapshot_print(print_html(tab), "html-issue92")


# tutorial.qmd: vectorized settings
tab <- tt(x) |>
  style_tt(
    j = 2:3,
    color = c("orange", "green"),
    background = "black")
expect_snapshot_print(print_html(tab), "html-vectorized_color_j")

# Issue #58
tab <- tt(iris[1:10,]) |> 
  style_tt(align = "c") |>
  group_tt(j = list("Sepal" = 1:2, "Petal" = 3:4))
expect_snapshot_print(print_html(tab), "html-issue58")

# Issue #88: indent
tab <- tt(iris[1:10,]) |> 
  style_tt(i = 2:4, indent = 3)
expect_snapshot_print(print_html(tab), "html-issue88")

# tutorial.qmd: heatmap
k <- data.frame(matrix(1:20, ncol = 5))
colnames(k) <- NULL
bg <- hcl.colors(20, "Inferno")
fg <- ifelse(as.matrix(k) < 17, tail(bg, 1), head(bg, 1))
fs <- 1:20
tab <- tt(k, width = .5, theme = "void") |>
  style_tt(
    i = 1:4,
    j = 1:5,
    color = fg,
    background = bg,
    fontsize = fs)
expect_snapshot_print(print_html(tab), "html-heatmap")

#Caption
tab <- tt(mtcars[1:3, 1:3],caption = "Blah blah")
expect_snapshot_print(print_html(tab), "html-caption")

# Footnote
tab <- tt(mtcars[1:3, 1:3],notes = list(a = "Blah.", b = "Blah blah."))
expect_snapshot_print(print_html(tab), "html-footnote")

# Style individual cells
tab <- tt(mtcars[1:4, 1:4]) |>
  style_tt(
    i = 2:3,
    j = c(1, 3, 4),
    italic = TRUE,
    background = "pink",
    color = "orange")
expect_snapshot_print(print_html(tab), "html-individual_cells")

# Issue #432: S4 refactor broke bootstrap_css argument
x <- mtcars[1:4, 1:5]
tab <- tt(x) |> style_tt(j = 1, bootstrap_css = "font-weight: bold; color: red;")
expect_snapshot_print(print_html(tab), "html-bootstrap_css")

# Line break
d <- data.frame(
  "{Sed ut \\\\ perspiciatis unde}",
  "dicta sunt<br> explicabo. Nemo"
) |> setNames(c("LaTeX line break", "HTML line break"))
d <-tt(d)
expect_snapshot_print(print_html(d), "html-line_break")

# Formatting
dat <- data.frame(w = c(143002.2092, 201399.181, 100188.3883),
                  x = c(1.43402, 201.399, 0.134588),
                  y = as.Date(c(999,675,3), origin = "1970-01-01"),
                  z = c(TRUE, TRUE, FALSE))
dat<-tt(dat, digits = 2)
expect_snapshot_print(print_html(dat), "html-formatting")

# Missing value replacement
tab <- data.frame(a = c(NA, 1, 2), b = c(3, NA, 5))
tab<-tt(tab)
tab<-format_tt(tab, replace = "-")
expect_snapshot_print(print_html(tab), "html-missing_value")

# Alignment
dat <- data.frame(
  a = c("a", "aa", "aaa"),
  b = c("b", "bb", "bbb"),
  c = c("c", "cc", "ccc"))
dat<-tt(dat) |> style_tt(j = 1:3, align = "lcr")
expect_snapshot_print(print_html(dat), "html-alignment")

# Font size
x <- mtcars[1:4, 1:5]
x <-tt(x) |> style_tt(j = "mpg|hp|qsec", fontsize = 1.5)
expect_snapshot_print(print_html(x), "html-font_size")

# Merge cells
x <-(mtcars[1:4, 1:5])
x <-tt(x)|> style_tt(
  i = 2, j = 2,
  colspan = 3,
  rowspan = 2,
  align = "c",
  alignv = "m",
  color = "white",
  background = "black",
  bold = TRUE)
expect_snapshot_print(print_html(x), "html-merge_cells")

# Spanning cells 
tab <- aggregate(mpg ~ cyl + am, FUN = mean, data = mtcars)
tab <- tab[order(tab$cyl, tab$am),]
tab<-tt(tab, digits = 2) |>
  style_tt(i = c(1, 3, 5), j = 1, rowspan = 2, alignv = "t")
expect_snapshot_print(print_html(tab), "html-spanning_cells")

# Omit headers
k <- (mtcars[1:4, 1:5])
colnames(k) <- NULL
k<-tt(k)
expect_snapshot_print(print_html(k), "html-omit_headers")

# Conditional styling
k <- mtcars[1:10, c("mpg", "am", "vs")]
k <-tt(k) |>
  style_tt(
    i = which(k$am == k$vs),
    background = "teal",
    color = "white")
expect_snapshot_print(print_html(k), "html-conditional_styling")

# Borders
x<-tt(mtcars[1:4, 1:5], theme = "void") |>
  style_tt(
    i = 0:3,
    j = 1:3,
    line = "tblr",
    line_width = 0.4,
    line_color = "orange")
expect_snapshot_print(print_html(x), "html-borders")

# Images
dat <- data.frame(
  Species = c("Spider", "Squirrel"),
  Image = ""
)
img <- c(
  "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSeLPSPrPtVgPg6BLCiN6lBYy8l1xNy0T5yttVjkIk0L3Rva8Zl",
  "https://encrypted-tbn2.gstatic.com/images?q=tbn:ANd9GcQdBlFVajljNz5qMbO622ihkIU2r6yA5whM9b8MbRGKOfJ8_UmZ"
)
dat<-tt(dat) |>
  plot_tt(j = 2, images = img, height = 3)
expect_snapshot_print(print_html(dat), "html-images")

# # Built-in plots
# # cannot be tested because the names of plots are random and set seed doesn't work
# set.seed(1024)
# plot_data <- list(mtcars$mpg, mtcars$hp, mtcars$qsec)
# dat <- data.frame(
#   Variables = c("mpg", "hp", "qsec"),
#   Histogram = "",
#   Density = "",
#   Bar = "",
#   Line = ""
# )
# lines <- lapply(1:3, \(x) data.frame(x = 1:10, y = rnorm(10)))
# dat<-tt(dat) |>
#   plot_tt(j = 2, fun = "histogram", data = plot_data) |>
#   plot_tt(j = 3, fun = "density", data = plot_data, color = "darkgreen") |>
#   plot_tt(j = 4, fun = "bar", data = list(2, 3, 6), color = "orange") |>
#   plot_tt(j = 5, fun = "line", data = lines, color = "blue") |>
#   style_tt(j = 2:5, align = "c")
# expect_snapshot_print(print_html(dat), "html-built_in_plots")

options(tinytable_print_output = NULL)
