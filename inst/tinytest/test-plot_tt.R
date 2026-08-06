# issue #95: plot_tt() can select j with a regex
tab <- data.frame(a = 1, b = 2) |>
  tt() |>
  plot_tt(1, j = "a", fun = "density", data = list(rnorm(100)))
expect_inherits(tab, "tinytable")

# An erroring plot function must not leak an open graphics device
bad_fun <- function(d, ...) function() stop("boom in user plot function")
tab <- tt(data.frame(x = 1, plot = "")) |>
  plot_tt(j = 2, fun = bad_fun, data = list(1))
n_dev <- length(grDevices::dev.list())
expect_error(save_tt(tab, "markdown"), pattern = "boom in user plot function")
expect_equal(length(grDevices::dev.list()), n_dev)
