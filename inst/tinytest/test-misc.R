source("helpers.R")

if (isTRUE(requiet("marginaleffects"))) {
  mod <- lm(mpg ~ hp * qsec, mtcars)
  s <- avg_slopes(mod)
  k <- tt(s) |> save_tt("dataframe")
  expect_equal(nrow(k), 2)
  expect_true(all(c("term", "estimate", "std.error") %in% colnames(k)))
}

# Issue #264: Row names global option
options(tinytable_tt_rownames = TRUE)
x <- mtcars[1:4, 1:4]
tab <- tt(x)
expect_equal(nrow(tab), 4)
expect_equal(ncol(tab), 5)
expect_equal(colnames(tab)[1], "rowname")
options(tinytable_tt_rownames = NULL)
