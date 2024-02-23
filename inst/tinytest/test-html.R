
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


options(tinytable_print_output = NULL)
