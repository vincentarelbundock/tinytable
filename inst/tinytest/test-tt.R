source("helpers.R")

# Issue #414
df = data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
colnames(df) = NULL
expect_equal(length(colnames(tt(df, rownames = TRUE))), 0)
expect_equal(length(colnames(tt(df))), 0)
