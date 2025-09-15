source("helpers.R")

# Issue #414
df1 = df2 = data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
colnames(df1) = NULL
expect_equal(length(colnames(tt(df1, rownames = TRUE))), 0)
expect_equal(length(colnames(tt(df1))), 0)

tab1 = save_tt(tt(df1), "markdown")
tab2 = save_tt(tt(df2, colnames = FALSE), "markdown")
expect_equal(tab1, tab2)


# colnames="label"
dat <- mtcars[1:5, c("cyl", "mpg", "hp")]
attr(dat$cyl, "label") <- "Cylinders"
attr(dat$mpg, "label") <- "Miles per Gallon"
attr(dat$hp, "label") <- "Horse Power"
tab <- tt(dat, colnames = "label") |> save_tt("dataframe")
expect_equal(unlist(tab[1, ]), c("Cylinders", "Miles per Gallon", "Horse Power"))
