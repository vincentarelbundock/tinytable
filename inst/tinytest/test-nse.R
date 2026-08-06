source("helpers.R")
using("tinysnapshot")

# Issue: NSE predicates must evaluate against the original typed data, not the
# character-coerced @data_body ("5" > "30" is TRUE lexicographically).
dat_num <- data.frame(x = c(5, 20, 100))
tab <- tt(dat_num) |> style_tt(i = x > 30, bold = TRUE)
tab <- build_tt(tab, "markdown")
expect_equal(tab@style$i[which(tab@style$bold)], 3)
expect_true(grepl("**100**", tab@table_string, fixed = TRUE))
expect_false(grepl("**5**", tab@table_string, fixed = TRUE))

# Same predicate via format_tt()
tab <- tt(dat_num) |> format_tt(i = x > 30, sprintf = "[%s]")
tab <- build_tt(tab, "markdown")
expect_true(grepl("[100]", tab@table_string, fixed = TRUE))
expect_false(grepl("[5]", tab@table_string, fixed = TRUE))

# Issue: with a plain data frame input, NSE expressions must be evaluated in
# the user's calling frame, so function-local variables resolve.
fn <- function() {
  myrows <- 2
  format_tt(data.frame(x = c(1.111, 2.222, 3.333)), i = myrows, digits = 2)
}
out <- fn()
expect_equivalent(out$x, c("1.111", "2.2", "3.333"))

# Issue: NSE row predicates are evaluated against pre-group data rows, but
# numeric `i` is resolved in final-table coordinates after group_tt() inserts
# label rows. Predicate-derived indices must be remapped through @index_body.
dat <- data.frame(x = 1:6, y = letters[1:6])

get_bg_rows <- function(tab) {
  so <- tab@style_other
  sort(unique(so$i[!is.na(so$background)]))
}

# group_tt() before style_tt()
tab <- tt(dat) |>
  group_tt(i = list("G" = 4)) |>
  style_tt(i = x == 5, background = "pink")
tab <- build_tt(tab, "markdown")
# group label at final row 4; data row x == 5 sits at final row 6
expect_equal(get_bg_rows(tab), 6)

# style_tt() before group_tt(): same result
tab <- tt(dat) |>
  style_tt(i = x == 5, background = "pink") |>
  group_tt(i = list("G" = 4))
tab <- build_tt(tab, "markdown")
expect_equal(get_bg_rows(tab), 6)

# Unchanged behavior: explicit numeric `i` refers to final-table positions
tab <- tt(dat) |>
  group_tt(i = list("G" = 4)) |>
  style_tt(i = 5, background = "pink")
tab <- build_tt(tab, "markdown")
expect_equal(get_bg_rows(tab), 5)

# No groups: remap is a no-op
tab <- tt(dat) |> style_tt(i = x == 5, background = "pink")
tab <- build_tt(tab, "markdown")
expect_equal(get_bg_rows(tab), 5)

# format_tt() shares the remap: predicate targets the x == 5 row after grouping
tab <- tt(dat) |>
  group_tt(i = list("G" = 4)) |>
  format_tt(i = x == 5, sprintf = "[%s]")
tab <- build_tt(tab, "markdown")
expect_true(grepl("[5]", tab@table_string, fixed = TRUE))
expect_false(grepl("[4]", tab@table_string, fixed = TRUE))

# plot_tt() shares the remap: image lands on the x == 5 row after grouping
tab <- tt(dat) |>
  group_tt(i = list("G" = 4)) |>
  plot_tt(i = x == 5, j = 2, images = "fake.png", height = 1)
tab <- build_tt(tab, "markdown")
lines <- strsplit(tab@table_string, "\n")[[1]]
img_line <- grep("fake.png", lines, fixed = TRUE, value = TRUE)
expect_equal(length(img_line), 1L)
# the image must land in the row whose first column is 5, not 4
expect_true(grepl("^\\| 5 \\|", img_line))

# NSE predicate with logical variable from the calling frame is also remapped
sel <- dat$x == 5
tab <- tt(dat) |>
  group_tt(i = list("G" = 4)) |>
  style_tt(i = sel, background = "pink")
tab <- build_tt(tab, "markdown")
expect_equal(get_bg_rows(tab), 6)
