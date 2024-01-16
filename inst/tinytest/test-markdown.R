source("helpers.R")
using("tinysnapshot")

# markdown without labels
k = iris[1:10,]
colnames(k) <- NULL
expect_snapshot_print(
  tt(k, output = "markdown"),
  label = "markdown-nocolnames")

