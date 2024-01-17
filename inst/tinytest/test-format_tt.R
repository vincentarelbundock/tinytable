# pkgload::load_all()
N <- 10
dat <- data.frame(
     x = c(1.430, rnorm(N - 1, mean = 100000)),
     y = as.Date(sample(1:1000, N)),
     z = sample(c(TRUE, FALSE), N, replace = TRUE)
)

# pkgload::load_all()
# dat |> 
#   format_tt(digits = 2, num_suffix = TRUE) |>
#   tt()
