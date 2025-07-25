source("helpers.R")
using("tinysnapshot")
requiet("data.table")

dat1 <- structure(list(EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")), class = "data.frame", row.names = c(NA, -27L)) # noqa
DT1 <- as.data.table(dat1)
colnames(DT1) <- NULL
tab <- tt(DT1)
expect_inherits(tab, "tinytable")
