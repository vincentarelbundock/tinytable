source("helpers.R")
using("tinysnapshot")

x <- data.frame(
  short = c("H1", "H2", "H3"),
  hypothesis = c(
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit",
    "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua",
    "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi"
  )
)

# tt(x, width = c(.2, .4)) |>
#     style_tt(i = 0:4, line = "tblr") |>
#     print("latex")
