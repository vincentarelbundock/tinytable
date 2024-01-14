source("helpers.R")
using("tinysnapshot")


pkgload::load_all()
k = iris[1:10,]
colnames(k) <- NULL
expect_snapshot_print(tt(k), "markdown-nocolnames")
