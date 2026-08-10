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

# Issue #673: `alignv` moves inline plot images in LaTeX via \raisebox
dat673 <- data.frame(Metric = c("Sales", "Growth"), Value = c(75, 92))
mk673 <- function(...) {
  out <- tt(dat673) |>
    plot_tt(j = 2, fun = "bar", data = as.list(dat673$Value), height = 5)
  out <- style_tt(out, ...)
  as.character(build_tt(out, "latex")@table_string)
}
# alignv on the text column applies to the whole row's image (as filed in #673)
expect_true(grepl("\\raisebox{-0.5\\height}{\\includegraphics", mk673(j = 1, alignv = "m"), fixed = TRUE))
# alignv on the plot column itself
expect_true(grepl("\\raisebox{\\dimexpr-\\height+\\ht\\strutbox\\relax}{\\includegraphics", mk673(j = 2, alignv = "t"), fixed = TRUE))
# bottom alignment and the default are unchanged
expect_false(grepl("raisebox", mk673(j = 1, alignv = "b"), fixed = TRUE))
expect_false(grepl("raisebox", mk673(j = 1, bold = TRUE), fixed = TRUE))
# cell-level style wraps only the targeted cell
one <- mk673(i = 1, j = 2, alignv = "m")
expect_equal(lengths(regmatches(one, gregexpr("raisebox", one))), 1L)
