# pkgload::load_all()
# N <- 10
# dat <- data.frame(
#      x = c(1.430, rnorm(N - 1, mean = 100000)),
#      y = as.Date(sample(1:1000, N)),
#      z = sample(c(TRUE, FALSE), N, replace = TRUE)
# )
#
# Q
# pkgload::load_all()
# k = dat |> tt("latex") |>
#   style_tt(color = "orange") |>
#   format_tt(j = 1, digits = 10, num_fmt = "decimal") |>
#   format_tt(j = 2, date = "%Y")
# print(k)
