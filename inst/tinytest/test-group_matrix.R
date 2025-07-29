source("helpers.R")
using("tinysnapshot")

# Basic matrix insertion - single position, single row
rowmat <- matrix(c("Inserted", "Row", "Data", "Here", "Extra"), nrow = 1)
tab <- tt(head(iris, 5)) |> group_tt(i = 3, j = rowmat)
t <- expect_table(tab)
expect_snapshot_print(
  t[["markdown"]],
  "group_matrix-single_position_single_row.md"
)
expect_snapshot_print(
  t[["latex"]],
  "group_matrix-single_position_single_row.tex"
)
expect_snapshot_print(
  t[["typst"]],
  "group_matrix-single_position_single_row.typ"
)
expect_snapshot_print(
  t[["html"]],
  "group_matrix-single_position_single_row.html"
)

# Matrix insertion - multiple positions and rows (covers both multiple positions and multiple rows)
rowmat <- matrix(
  c(
    "Row1",
    "At",
    "Pos1",
    "Data",
    "A",
    "Row2",
    "At",
    "Pos1",
    "Data",
    "B",
    "Row3",
    "At",
    "Pos2",
    "Data",
    "C"
  ),
  nrow = 3,
  byrow = TRUE
)
tab <- tt(head(iris, 7)) |> group_tt(i = c(2, 2, 5), j = rowmat)
t <- expect_table(tab)
expect_snapshot_print(
  t[["markdown"]],
  "group_matrix-multiple_positions_multiple_rows.md"
)
expect_snapshot_print(
  t[["latex"]],
  "group_matrix-multiple_positions_multiple_rows.tex"
)
expect_snapshot_print(
  t[["typst"]],
  "group_matrix-multiple_positions_multiple_rows.typ"
)
expect_snapshot_print(
  t[["html"]],
  "group_matrix-multiple_positions_multiple_rows.html"
)

# Matrix insertion at edge positions (combines position 1 and last position)
expect_error(
  tt(head(iris, 4)) |>
    group_tt(
      i = 1,
      j = matrix(c("Header", "Row", "At", "Top", "Position"), nrow = 1)
    ) |>
    group_tt(
      i = 6,
      j = matrix(c("Footer", "Row", "At", "Bottom", "Position"), nrow = 1)
    ),
  pattern = "Only one group row"
)

# Matrix with single column reshape and styling combined
rowmat <- matrix(c("A", "B", "C", "D", "E"))
tab <- tt(head(iris, 4)) |>
  group_tt(i = 2, j = rowmat) |>
  style_tt(i = "groupi", background = "lightblue")
t <- expect_table(tab)
expect_snapshot_print(
  t[["markdown"]],
  "group_matrix-single_column_with_styling.md"
)
expect_snapshot_print(
  t[["latex"]],
  "group_matrix-single_column_with_styling.tex"
)
expect_snapshot_print(
  t[["typst"]],
  "group_matrix-single_column_with_styling.typ"
)
expect_snapshot_print(
  t[["html"]],
  "group_matrix-single_column_with_styling.html"
)

# Matrix row duplication - single row matrix with multiple positions
rowmat <- matrix(colnames(iris))
tab <- tt(head(iris, 7)) |> group_tt(i = c(2, 5), j = rowmat)
t <- expect_table(tab)
expect_snapshot_print(t[["markdown"]], "group_matrix-row_duplication.md")
expect_snapshot_print(t[["latex"]], "group_matrix-row_duplication.tex")
expect_snapshot_print(t[["typst"]], "group_matrix-row_duplication.typ")
expect_snapshot_print(t[["html"]], "group_matrix-row_duplication.html")
