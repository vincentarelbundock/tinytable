
source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "html")


clean <- function(x) {
  x <- knitr::knit_print(x)
  x <- gsub("tinytable_\\w+\\b", "tinytable", x)
  x <- gsub("styleCell_\\w+\\b", "tinytable", x)
  x
}

x <- mtcars[1:4, 1:5]

tab <- tt(x, theme = "striped")
expect_equal_to_reference(clean(tab), "_tinysnapshot/html-striped.rds")

tab <- tt(x, theme = "striped") |>
  style_tt(color = "orange")
expect_equal_to_reference(clean(tab), "_tinysnapshot/html-striped_orange.rds")

# tutorial.qmd: vectorized settings
tab <- tt(x) |>
  style_tt(
    j = 2:3,
    color = c("orange", "green"),
    background = "black")
expect_equal_to_reference(clean(tab), "_tinysnapshot/html-vectorized_color_j.rds")

# Issue #58
tab <- tt(iris[1:10,]) |> 
  style_tt(align = "c") |>
  group_tt(j = list("Sepal" = 1:2, "Petal" = 3:4))
expect_equal_to_reference(clean(tab), "_tinysnapshot/html-issue58.rds")


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
expect_equal_to_reference(clean(tab), "_tinysnapshot/html-heatmap.rds")


options(tinytable_print_output = NULL)
