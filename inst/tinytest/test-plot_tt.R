# issue #95: plot_tt() can select j with a regex
tab <- data.frame(a = 1, b = 2) |>
  tt() |>
  plot_tt(1, j = "a", fun = "density", data = list(rnorm(100)))
expect_inherits(tab, "tinytable")
