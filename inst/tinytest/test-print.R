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
