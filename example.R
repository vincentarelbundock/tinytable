
source("R/latex.R")
x <- mtcars[1:4, 1:5]
init_latex(x) |>  
  style_columns_latex(index = 1:2, halign = "c", bg = "pink", bold = TRUE) |>
  style_columns_latex(index = 4, halign = "r", fg = "blue") |>
  style_rows_latex(index = 4, fg = "yellow") |>
  cat()
    


pkgload::load_all("~/repos/kableExtra")
kbl(x, format = "latex", tabular = "tblr")

