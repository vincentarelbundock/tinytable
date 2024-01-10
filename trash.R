pkgload::load_all()
mtcars[1:5, 1:5] |>
  tt(output = "latex") |>
  cat()
  # style_tt(align = "c", color = "white", background = "black")
