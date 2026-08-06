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


# Regression: colnames = FALSE must not create a phantom header row (i = 0)
# or duplicate body row 1 in @style_other
x <- build_tt(tt(mtcars[1:3, 1:3], colnames = FALSE), "html")
expect_equal(nrow(x@style_other), 9)
expect_equal(sort(unique(x@style_other$i)), c(1, 2, 3))
expect_false(any(duplicated(x@style_other[, c("i", "j")])))

# header index sequences unchanged for nhead = 1 and nhead = 2
x1 <- build_tt(tt(mtcars[1:3, 1:3]), "html")
expect_equal(sort(unique(x1@style_other$i)), c(0, 1, 2, 3))
x2 <- build_tt(tt(mtcars[1:3, 1:3]) |> group_tt(j = list("G" = 1:2)), "html")
expect_equal(sort(unique(x2@style_other$i)), c(-1, 0, 1, 2, 3))

# Regression: as.character() errored on fresh tables because @output was
# still "tinytable"
out <- as.character(tt(mtcars[1:2, 1:2]))
expect_inherits(out, "character")
expect_true(grepl("mpg", out))

# Regression: colnames<- errored on tables created with colnames = FALSE,
# and did not restore the header row
y <- tt(mtcars[1:3, 1:3], colnames = FALSE)
colnames(y) <- c("a", "b", "c")
expect_equal(colnames(y), c("a", "b", "c"))
expect_equal(y@nhead, 1)
expect_true(grepl("a", save_tt(y, "markdown")))
y <- tt(mtcars[1:3, 1:3], colnames = FALSE)
names(y) <- c("a", "b", "c")
expect_equal(names(y), c("a", "b", "c"))
expect_equal(y@nhead, 1)
# wrong length still errors
y <- tt(mtcars[1:3, 1:3], colnames = FALSE)
expect_error(colnames(y) <- c("a", "b"))
