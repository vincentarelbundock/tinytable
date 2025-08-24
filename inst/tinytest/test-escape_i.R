source("helpers.R")
using("tinysnapshot")
options(tinytable_print_output = "latex")

# Create a LaTeX table with $ signs in each element type
# Testing format_tt(escape = TRUE) with different target elements

# Base data with $ signs in all elements
dat <- data.frame(
  "column$one" = c("data$1", "value$2"),
  "column$two" = c("cost$100", "price$50"),
  check.names = FALSE
)

# Create base table with all elements containing $ signs
tab <- tt(dat,
          caption = "Budget $100 analysis",
          notes = "Cost data $50-$100") |>
  group_tt(i = list("Section$A" = 1:2)) |>
  group_tt(j = list("Price$Data" = 1:2))

# No escaping (baseline)
t1 <- expect_table(tab, formats = "latex")
expect_snapshot_print(t1[["latex"]], "escape_i-no_escape.tex")

# Escape all content (default behavior)
t2 <- expect_table(tab |> format_tt(escape = TRUE), formats = "latex")
expect_snapshot_print(t2[["latex"]], "escape_i-escape_all.tex")

# Escape only column names
t3 <- expect_table(tab |> format_tt("colnames", escape = TRUE), formats = "latex")
expect_snapshot_print(t3[["latex"]], "escape_i-escape_colnames.tex")

# Escape row groups
t4 <- expect_table(tab |> format_tt("groupi", escape = TRUE), formats = "latex")
expect_snapshot_print(t4[["latex"]], "escape_i-escape_groupi.tex")

# Escape column groups
t5 <- expect_table(tab |> format_tt("groupj", escape = TRUE), formats = "latex")
expect_snapshot_print(t5[["latex"]], "escape_i-escape_groupj.tex")

# Escape caption
t6 <- expect_table(tab |> format_tt("caption", escape = TRUE), formats = "latex")
expect_snapshot_print(t6[["latex"]], "escape_i-escape_caption.tex")

# Escape notes
t7 <- expect_table(tab |> format_tt("notes", escape = TRUE), formats = "latex")
expect_snapshot_print(t7[["latex"]], "escape_i-escape_notes.tex")

# Selective escaping - multiple elements
t8 <- expect_table(tab |> 
                   format_tt("colnames", escape = TRUE) |>
                   format_tt("caption", escape = TRUE), 
                   formats = "latex")
expect_snapshot_print(t8[["latex"]], "escape_i-selective_escape.tex")

# All elements escaped
t9 <- expect_table(tab |> 
                   format_tt(c("colnames", "groupi", "groupj", "caption", "notes"), escape = TRUE),
                   formats = "latex")
expect_snapshot_print(t9[["latex"]], "escape_i-escape_all_elements.tex")
