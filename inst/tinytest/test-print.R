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

# Checking `gfm` printing
orig_option <- getOption("tinytable_markdown_hlines")

options(tinytable_markdown_hlines = TRUE)
void <- capture.output(x <- print(tab, "gfm"))

options(tinytable_markdown_hlines = FALSE)
void <- capture.output(x2 <- print(tab, "gfm"))
expect_equal(x@table_string, x2@table_string)
expect_false(getOption("tinytable_markdown_hlines")) # check restored option

options(tinytable_markdown_hlines = orig_option)
