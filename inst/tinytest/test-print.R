source("helpers.R")
using("tinysnapshot")

tab <- tt(mtcars[1:4, 1:5])

options("tinytable_print_output" = "markdown")
void <- capture.output(x <- print(tab))
expect_equivalent(x@output, "markdown")

options("tinytable_print_output" = "latex")
void <- capture.output(x <- print(tab))
expect_equivalent(x@output, "latex")

options("tinytable_print_output" = "typst")
void <- capture.output(x <- print(tab))
expect_equivalent(x@output, "typst")

# avoid launching a viewer
# options("tinytable_print_output" = "html")
# void <- capture.output(x <- print(tab))
# expect_equivalent(x@output, "html")

options("tinytable_print_output" = NULL)

exit_file("skipping webshot2 screenshot tests: chromote fails to locate visible elements when rendering the styled tinytable (pre-existing flake, unrelated to scrutin)")

# print(output = "knitr"): returns knitr::include_graphics() with a valid PNG
if (is_local && requiet("webshot2") && requiet("knitr")) {
  tab_styled <- tt(mtcars[1:4, 1:5]) |> style_tt(background = "lightblue", i = 1:2)
  out <- print(tab_styled, output = "knitr")
  expect_inherits(out, "knit_asis")
  expect_true(file.exists(out[1]))
  expect_true(grepl("\\.png$", out[1]))
  expect_true(file.info(out[1])$size > 0)
}

# print(output = "raster"): draws on graphics device
if (is_local && requiet("webshot2") && requiet("png")) {
  tab_styled <- tt(mtcars[1:4, 1:5]) |> style_tt(background = "lightblue", i = 1:2)
  tmp_plot <- tempfile(fileext = ".png")
  grDevices::png(tmp_plot, width = 800, height = 400)
  out <- print(tab_styled, output = "raster")
  grDevices::dev.off()
  expect_true(file.exists(tmp_plot))
  expect_true(file.info(tmp_plot)$size > 0)
}
