source("helpers.R")

tt.lm <- function(x, ...) {
  results <- broom::tidy(x)
  tt(results, ...)
}

tab <- lm(mpg ~ hp, data = mtcars) |> tt()
expect_inherits(tab, "tinytable")
expect_equal(nrow(tab), 2)
expect_equal(ncol(tab), 5)

tab <- lm(mpg ~ hp + factor(cyl), data = mtcars) |> tt()
expect_equal(nrow(tab), 4)
expect_equal(ncol(tab), 5)

rm("tt.lm")
