source("helpers.R")
using("tinysnapshot")

# `j` refers to a position in the rendered table, so subsetting does not shift
# stored indices; entries that fall outside the narrower table are dropped and
# must not error at build.
w <- style_tt(tt(mtcars[1:3, 1:4]), j = 4, color = "red")
s <- subset(w, select = 1:2)
expect_equal(length(s@lazy_style), 0L)
out <- build_tt(s, "html")
expect_inherits(out, "tinytable")
expect_false(grepl("red", out@table_string, fixed = TRUE))

# A style on a kept column survives and targets the correct column.
w <- style_tt(tt(mtcars[1:3, 1:4]), j = 1, color = "red")
s <- subset(w, select = 1:2)
out <- build_tt(s, "html")
sty <- out@style[!is.na(out@style$color) & out@style$color == "red", ]
expect_true(nrow(sty) > 0)
expect_true(all(sty$j == 1))

# A style keeps its position in the rendered table: it does not follow the
# column it originally pointed to.
w <- style_tt(tt(mtcars[1:3, 1:4]), j = 2, color = "blue")
s <- subset(w, select = 2:4)
expect_equal(s@lazy_style[[1]][["j"]], 2)
out <- build_tt(s, "html")
sty <- out@style[!is.na(out@style$color) & out@style$color == "blue", ]
expect_true(all(sty$j == 2))

# Issue: drop = TRUE broke S4 slot assignment when selecting a single column.
# It now warns and is ignored.
expect_warning(
  subset(tt(mtcars[1:3, 1:3]), select = 1, drop = TRUE),
  pattern = "not supported"
)
s <- suppressWarnings(subset(tt(mtcars[1:3, 1:3]), select = 1, drop = TRUE))
expect_inherits(s, "tinytable")
expect_equal(s@ncol, 1L)
expect_inherits(build_tt(s, "markdown"), "tinytable")

# Issue: character select produced an all-NA index vector, silently remapping
# stored lazy calls to every column. Names are now matched and validated.
x <- format_tt(tt(mtcars[1:3, 1:3]), j = 1, digits = 0)
y <- subset(x, select = "mpg")
expect_equal(y@lazy_format[[1]][["j"]], 1L)
out <- build_tt(y, "markdown")
expect_true(grepl("21.0", out@table_string, fixed = TRUE))

# character select leaves in-range indices alone
x <- format_tt(tt(mtcars[1:3, 1:3]), j = 2, digits = 0)
y <- subset(x, select = c("cyl", "disp"))
expect_equal(y@lazy_format[[1]][["j"]], 2)

# unknown column names error informatively instead of corrupting state
expect_error(
  subset(tt(mtcars[1:3, 1:3]), select = "nope"),
  pattern = "nope"
)

# Colspan entries (e.g. group_tt(i = ...) row labels, which style i = row,
# j = 1, colspan = ncol) survive and are clipped to the narrower table.
w <- tt(mtcars[1:5, 1:4]) |> group_tt(i = list("grp" = 3))
s <- subset(w, select = -1)
expect_equal(length(s@lazy_style), 1L)
expect_equal(s@lazy_style[[1]][["j"]], 1)
expect_equal(s@lazy_style[[1]][["colspan"]], 3L)
expect_inherits(build_tt(s, "html"), "tinytable")

# Negative j is a selection against the rendered table, so it is left alone.
x <- format_tt(tt(mtcars[1:3, 1:4]), j = -1, digits = 0)
y <- subset(x, select = 2:3)
expect_equal(y@lazy_format[[1]][["j"]], -1)

# Column spans stay where they are and are clipped at the new right edge:
# "AAA" keeps its two columns, "BBB" is reduced to one.
g <- tt(mtcars[1:3, 1:4]) |> group_tt(j = list("AAA" = 1:2, "BBB" = 3:4))
s <- subset(g, select = -2)
expect_equal(unname(unlist(s@group_data_j[1, ])), c("AAA", "", "BBB"))
spans <- s@lazy_style[vapply(
  s@lazy_style,
  function(e) isTRUE(e[["colspan"]] > 1),
  logical(1)
)]
expect_equal(length(spans), 1L)
expect_equal(spans[[1]][["j"]], 1)
expect_equal(spans[[1]][["colspan"]], 2L)

# A span whose anchor falls outside the subsetted table disappears.
s <- subset(g, select = 1:2)
expect_equal(unname(unlist(s@group_data_j[1, ])), c("AAA", ""))
expect_inherits(build_tt(s, "html"), "tinytable")
