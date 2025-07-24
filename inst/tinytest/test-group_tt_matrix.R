source("helpers.R")
using("tinysnapshot")

# Define all output formats to test
all_formats <- list(
  list(option = "markdown", ext = "md", special = FALSE),
  list(option = "latex", ext = "tex", special = FALSE),
  list(option = "typst", ext = "typ", special = FALSE),
  list(option = "html", ext = "html", special = TRUE)
)

# Helper function to run test with all formats
run_test_all_formats <- function(table_func, base_label) {
  for (fmt in all_formats) {
    if (fmt$special) {
      options(tinytable_print_output = NULL)
      tab <- table_func()
      expect_snapshot_print(print_html(tab), paste0(base_label, ".", fmt$ext))
    } else {
      options(tinytable_print_output = fmt$option)
      tab <- table_func()
      expect_snapshot_print(tab, label = paste0(base_label, ".", fmt$ext))
    }
  }
}

# Basic matrix insertion - single position, single row
run_test_all_formats(function() {
  rowmat <- matrix(c("Inserted", "Row", "Data", "Here", "Extra"), nrow = 1)
  tt(head(iris, 5)) |> group_tt(i = 3, j = rowmat)
}, "group_tt_matrix-single_position_single_row")

# Matrix insertion - multiple positions and rows (covers both multiple positions and multiple rows)
run_test_all_formats(function() {
  rowmat <- matrix(c(
    "Row1", "At", "Pos1", "Data", "A",
    "Row2", "At", "Pos1", "Data", "B",
    "Row3", "At", "Pos2", "Data", "C"
  ), nrow = 3, byrow = TRUE)
  tt(head(iris, 7)) |> group_tt(i = c(2, 2, 5), j = rowmat)
}, "group_tt_matrix-multiple_positions_multiple_rows")

# Matrix insertion at edge positions (combines position 1 and last position)
run_test_all_formats(function() {
  tt(head(iris, 4)) |>
    group_tt(i = 1, j = matrix(c("Header", "Row", "At", "Top", "Position"), nrow = 1)) |>
    group_tt(i = 6, j = matrix(c("Footer", "Row", "At", "Bottom", "Position"), nrow = 1))
}, "group_tt_matrix-edge_positions")

# Matrix with single column reshape and styling combined
run_test_all_formats(function() {
  rowmat <- matrix(c("A", "B", "C", "D", "E"), ncol = 1)
  tt(head(iris, 4)) |>
    group_tt(i = 2, j = rowmat) |>
    style_tt(i = "groupi", background = "lightblue")
}, "group_tt_matrix-single_column_with_styling")

# Prepare complex data once for region grouping tests
dat <- data.frame(
  Region = as.character(state.region),
  State = row.names(state.x77),
  state.x77[, 1:3]
) |>
  sort_by(~ Region + State) |>
  subset(Region %in% c("North Central", "Northeast"))
dat <- do.call(rbind, by(dat, dat$Region, head, n = 3))
row.names(dat) <- NULL

# Comprehensive region grouping test (covers both colnames=TRUE/FALSE and different grouping patterns)
colnames_options <- c(TRUE, FALSE)
for (use_colnames in colnames_options) {
  suffix <- if (use_colnames) "with_colnames" else "no_colnames"

  run_test_all_formats(function() {
    tt(dat, colnames = use_colnames) |>
      group_tt(i = list("North Central" = 1, "Northeast" = 5), indent = 0) |>
      group_tt(i = c(1, 4), j = matrix(colnames(dat)))
  }, paste0("group_tt_matrix-region_grouping_", suffix))
}

# Essential styling configurations (reduced from 6 to 3 most representative cases)
essential_styling_configs <- list(
  # Test list grouping only
  list(colnames = FALSE, group_i_list = list("North Central" = 1, "Northeast" = 4), group_i_vec = NULL, style_i = c(1, 5), num = "list_only"),
  # Test vector grouping only
  list(colnames = TRUE, group_i_list = NULL, group_i_vec = c(1, 4), style_i = c(1, 5), num = "vector_only"),
  # Test combined grouping
  list(colnames = TRUE, group_i_list = list("North Central" = 1, "Northeast" = 5), group_i_vec = c(1, 4), style_i = c(1, 6), num = "combined")
)

# Run essential styling tests
for (config in essential_styling_configs) {
  run_test_all_formats(function() {
    tab <- tt(dat, colnames = config$colnames)

    if (!is.null(config$group_i_list)) {
      tab <- tab |> group_tt(i = config$group_i_list, indent = 0)
    }

    if (!is.null(config$group_i_vec)) {
      tab <- tab |> group_tt(i = config$group_i_vec, j = matrix(colnames(dat)))
    }

    tab |> style_tt(i = config$style_i, color = "red")
  }, paste0("group_tt_matrix-styled_", config$num))
}

# Reset options
options(tinytable_print_output = NULL)

