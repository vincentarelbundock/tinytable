
N <- 10
dat <- data.frame(
     x = c(1.430, rnorm(N - 1, mean = 100000)),
     y = as.Date(sample(1:1000, N)),
     z = sample(c(TRUE, FALSE), N, replace = TRUE)
)
pkgload::load_all()
k = format_tt(num_fmt = "decimal", digits = 3, num_mark_big = " ", num_mark_dec = ",")
