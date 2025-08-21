source("helpers.R")
using("tinysnapshot")

dat <- do.call(rbind, by(iris, ~Species, head, n = 2))
tab <- tt(dat) |>
  group_tt(i = Species) |>
  subset(select = -Species)
expect_snapshot_print(tab, label = "methods-groupi-subset.md")


tab <- tt(mtcars[1:4, ]) |>
  subset(select = c(am, mpg, cyl))
expect_snapshot_print(tab, label = "methods-subset_01.md")
