source("helpers.R")
using("tinysnapshot")

expect_error(
  tt(head(iris)) |> style_tt("groupj", color = "blue"),
  "not supported")
