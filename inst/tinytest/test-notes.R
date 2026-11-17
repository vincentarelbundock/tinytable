source("helpers.R")
using("tinysnapshot")

# latex table with cell marker table
options(tinytable_print_output = "latex")
x <- mtcars[1:3, 1:3]
n <- list("Blah blah", "*" = list(i = 0:1, j = 2, text = "foo bar"))
tab <- tt(x, notes = n)
expect_snapshot_print(tab, label = "notes-latex_cell_markers.tex")
options(tinytable_print_output = NULL)

# s4 refactor bug along the way
x <- mtcars[1:3, 1:3]
tab <- save_tt(tt(x, notes = "Hello World!"), "markdown")
expect_inherits(tab, "character")

# Issue #692: TeX collapses spaces, so unlabelled notes need distinct keys.
notes <- list("first", "second", "third", "fourth")
expected <- c(
  "note{}={first},",
  "note{{}}={second},",
  "note{{}{}}={third},",
  "note{{}{}{}}={fourth},"
)
for (n in list(notes, append(notes, list(a = "labelled"), after = 2))) {
  tab <- tt(data.frame(x = 1:3, y = c("A", "B", "C")), notes = n)
  out <- save_tt(tab, "latex")
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_identical(lines[lines %in% expected], expected)
  if (!is.null(names(n))) {
    expect_true("note{a}={labelled}," %in% lines)
  }
}
