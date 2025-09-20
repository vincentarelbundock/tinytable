source("helpers.R")

input <- style_vector(1:2, color = c("orange", "teal"))
output <- c(
  "<span style='color:#FFA500'>1</span>",
  "<span style='color:#008080'>2</span>")
expect_equivalent(input, output)
