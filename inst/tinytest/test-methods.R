source("helpers.R")
using("tinysnapshot")

tab <- mtcars |>
  sort_by(~am) |>
  tt() |>
  subset(mpg > 20) |>
  group_tt(am)
expect_snapshot_print(tab, label = "methods-subset.md")
