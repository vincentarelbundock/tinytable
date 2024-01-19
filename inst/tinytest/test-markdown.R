source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "markdown")

# markdown without labels
k = iris[1:10,]
colnames(k) <- NULL
expect_snapshot_print(
  tt(k), label = "markdown-nocolnames")



options(tinytable_print_output = NULL)
