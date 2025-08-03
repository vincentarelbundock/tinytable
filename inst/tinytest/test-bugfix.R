source("helpers.R")
using("tinysnapshot")
requiet("data.table")

dat1 <- structure(list(EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")), class = "data.frame", row.names = c(NA, -27L)) # noqa
DT1 <- as.data.table(dat1)
colnames(DT1) <- NULL
tab <- tt(DT1)
expect_inherits(tab, "tinytable")


# Bug group_tt(i) accepts factor variables
dat <- do.call(rbind, by(iris, iris$Species, head, n = 3))
tab <- tt(dat) |>
  group_tt(dat$Species) |>
  save_tt("markdown")
tab <- unlist(strsplit(tab, "\\n"))
expect_equal(sum(grepl("^\\| virginica", tab)), 1)
