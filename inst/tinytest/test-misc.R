source("helpers.R")

if (isTRUE(requiet("marginaleffects"))) {
  mod <- lm(mpg ~ hp * qsec, mtcars)
  s <- avg_slopes(mod)
  k <- tt(s) |> print("dataframe")
  expect_equal(nrow(k), 2)
  expect_true(all(c("term", "estimate", "std.error") %in% colnames(k)))
}
