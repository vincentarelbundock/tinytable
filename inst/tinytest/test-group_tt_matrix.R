source("helpers.R")
using("tinysnapshot")

# Basic matrix insertion - single position, single row
options(tinytable_print_output = "markdown")
rowmat <- matrix(c("Inserted", "Row", "Data", "Here", "Extra"), nrow = 1)
tab <- tt(head(iris, 5)) |>
  group_tt(i = 3, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-single_position_single_row.md")

# Basic matrix insertion - single position, multiple rows
rowmat <- matrix(c(
  "First", "Inserted", "Row", "Data", "A",
  "Second", "Inserted", "Row", "Data", "B"
), nrow = 2, byrow = TRUE)
tab <- tt(head(iris, 5)) |>
  group_tt(i = 2, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-single_position_multiple_rows.md")

# Matrix insertion - multiple positions, single row each
rowmat <- matrix(c("Inserted", "At", "Position", "Two", "X"), nrow = 1)
tab <- tt(head(iris, 7)) |>
  group_tt(i = c(2, 5), j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-multiple_positions_single_row.md")

# Matrix insertion - multiple positions, multiple rows
rowmat <- matrix(c(
  "Row1", "At", "Pos1", "Data", "A",
  "Row2", "At", "Pos1", "Data", "B",
  "Row3", "At", "Pos2", "Data", "C"
), nrow = 3, byrow = TRUE)
tab <- tt(head(iris, 7)) |>
  group_tt(i = c(2, 2, 5), j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-multiple_positions_multiple_rows.md")

# Matrix insertion at position 1 (top of table)
rowmat <- matrix(c("Header", "Row", "At", "Top", "Position"), nrow = 1)
tab <- tt(head(iris, 3)) |>
  group_tt(i = 1, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-position_one.md")

# Matrix insertion at last position (bottom of table)
rowmat <- matrix(c("Footer", "Row", "At", "Bottom", "Position"), nrow = 1)
tab <- tt(head(iris, 3)) |>
  group_tt(i = 4, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-position_last.md")

# Test with different output formats
# LaTeX format
options(tinytable_print_output = "latex")
rowmat <- matrix(c("LaTeX", "Inserted", "Row", "Data", "Format"), nrow = 1)
tab <- tt(head(iris, 3)) |>
  group_tt(i = 2, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-latex_format.tex")

# Typst format
options(tinytable_print_output = "typst")
rowmat <- matrix(c("Typst", "Inserted", "Row", "Data", "Format"), nrow = 1)
tab <- tt(head(iris, 3)) |>
  group_tt(i = 2, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-typst_format.typ")

# HTML format
options(tinytable_print_output = NULL)
rowmat <- matrix(c("HTML", "Inserted", "Row", "Data", "Format"), nrow = 1)
tab <- tt(head(iris, 3)) |>
  group_tt(i = 2, j = rowmat)
expect_snapshot_print(print_html(tab), "group_tt_matrix-html_format.html")

# Matrix insertion with single column that gets reshaped
options(tinytable_print_output = "markdown")
rowmat <- matrix(c("A", "B", "C", "D", "E"), ncol = 1)
tab <- tt(head(iris, 3)) |>
  group_tt(i = 2, j = rowmat)
expect_snapshot_print(tab, label = "group_tt_matrix-single_column_reshape.md")

# Matrix insertion combined with styling
options(tinytable_print_output = NULL)
rowmat <- matrix(c("Styled", "Inserted", "Row", "With", "Background"), nrow = 1)
tab <- tt(head(iris, 4)) |>
  group_tt(i = 2, j = rowmat) |>
  style_tt(i = "groupi", background = "lightblue")
expect_snapshot_print(print_html(tab), "group_tt_matrix-with_styling.html")

# Complex example with region grouping and column headers
# Test data from tmp.R
dat <- data.frame(
  Region = as.character(state.region),
  State = row.names(state.x77),
  state.x77[, 1:3]
) |>
  sort_by(~ Region + State) |>
  subset(Region %in% c("North Central", "Northeast"))
dat <- do.call(rbind, by(dat, dat$Region, head, n = 3))
row.names(dat) <- NULL

# Test markdown output
tab <- tt(dat, colnames = TRUE) |>
  group_tt(i = list("North Central" = 1, "Northeast" = 5), indent = 0) |>
  group_tt(i = c(1, 4), j = matrix(colnames(dat)))
options(tinytable_print_output = "markdown")
expect_snapshot_print(tab, label = "group_tt_matrix-region_grouping.md")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, label = "group_tt_matrix-region_grouping.tex")
options(tinytable_print_output = "html")
expect_snapshot_print(print_html(tab), "group_tt_matrix-region_grouping.html")

# Same examples with colnames=FALSE
tab <- tt(dat, colnames = FALSE) |>
  group_tt(i = list("North Central" = 1, "Northeast" = 5), indent = 0) |>
  group_tt(i = c(1, 4), j = matrix(colnames(dat)))
options(tinytable_print_output = "markdown")
expect_snapshot_print(tab, label = "group_tt_matrix-region_grouping_no_colnames.md")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, label = "group_tt_matrix-region_grouping_no_colnames.tex")
options(tinytable_print_output = NULL)
expect_snapshot_print(print_html(tab), "group_tt_matrix-region_grouping_no_colnames.html")


# Fancty table styling
dat <- data.frame(
  Region = as.character(state.region),
  State = row.names(state.x77),
  state.x77[, 1:3]
) |>
  sort_by(~ Region + State) |>
  subset(Region %in% c("North Central", "Northeast"))
dat <- do.call(rbind, by(dat, dat$Region, head, n = 3))
row.names(dat) = NULL

tt(dat, colnames = FALSE) |>
  group_tt(i = list("North Central" = 1, "Northeast" = 4), indent = 0) |>
  style_tt(i = c(1, 5), color = "red")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_01.tex")
options(tinytable_print_output = "typst")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_01.typ")

tt(dat, colnames = FALSE) |>
  group_tt(i = c(1, 4), j = matrix(colnames(dat))) |>
  style_tt(i = c(1, 5), color = "red")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_02.tex")
options(tinytable_print_output = "typst")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_02.typ")

tt(dat, colnames = FALSE) |>
  group_tt(i = list("North Central" = 1, "Northeast" = 5), indent = 0) |>
  group_tt(i = c(1, 4), j = matrix(colnames(dat))) |>
  style_tt(i = c(1, 6), color = "red")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_03.tex")
options(tinytable_print_output = "typst")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_03.typ")

tt(dat, colnames = TRUE) |>
  group_tt(i = list("North Central" = 1, "Northeast" = 4), indent = 0) |>
  style_tt(i = c(1, 5), color = "red")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_04.tex")
options(tinytable_print_output = "typst")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_04.typ")

tt(dat, colnames = TRUE) |>
  group_tt(i = c(1, 4), j = matrix(colnames(dat))) |>
  style_tt(i = c(1, 5), color = "red")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_05.tex")
options(tinytable_print_output = "typst")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_05.typ")

tt(dat, colnames = TRUE) |>
  group_tt(i = list("North Central" = 1, "Northeast" = 5), indent = 0) |>
  group_tt(i = c(1, 4), j = matrix(colnames(dat))) |>
  style_tt(i = c(1, 6), color = "red")
options(tinytable_print_output = "latex")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_06.tex")
options(tinytable_print_output = "typst")
expect_snapshot_print(tab, "group_tt_matrix-region_grouping_no_colnames_style_06.typ")


# Reset options
options(tinytable_print_output = NULL)
