source("helpers.R")
using("tinysnapshot")


# latex table with cell marker table
options(tinytable_print_output = "latex")
x <- mtcars[1:3, 1:3]
n <- list("Blah blah", "*" = list(i = 0:1, j = 2, text = "foo bar"))
tab <- tt(x, notes = n)
expect_snapshot_print(tab, label = "notes-latex_cell_markers")
options(tinytable_print_output = NULL)











