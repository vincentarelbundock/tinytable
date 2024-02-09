

x <- mtcars[1:3, 1:3]

tt(x, placement = "H") |> 
    style_tt(1, 1, rowspan = 2, colspan = 2, align = "c") |>
    print("latex")

tt(x, placement = "H") |> 
    style_tt(1, 1, rowspan = 2, colspan = 2, align = "c") |>
    save_tt("~/Downloads/trash.html", overwrite = TRUE)
