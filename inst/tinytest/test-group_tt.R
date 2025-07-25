source("helpers.R")
using("tinysnapshot")

# vector row labels
set.seed(48103)
dat <- data.frame(
  label = c("a", "a", "a", "b", "b", "c", "a", "a"),
  x1 = rnorm(8),
  x2 = rnorm(8)
)
tab <- tt(dat[, 2:3]) |> group_tt(i = dat$label)
tab@output <- "markdown"
expect_snapshot_print(tab, label = "group_tt-vector_row_labels.md")

# 3 level tests across formats
x <- mtcars[1:3, 1:5]
tab <- tt(x) |>
  group_tt(j = list("a" = 2:3, "b" = 4:5)) |>
  group_tt(j = list("c" = 1:2, "d" = 3:5)) |>
  group_tt(j = list("e" = 1:3, "f" = 4))

t <- expect_table(tab, formats = c("markdown", "latex", "typst"))
expect_snapshot_print(t[["markdown"]], "group_tt-3level.md")
expect_snapshot_print(t[["latex"]], "group_tt-3level.tex")
expect_snapshot_print(t[["typst"]], "group_tt-3level.typ")

tab <- tt(mtcars[1:10, 1:5]) |>
  group_tt(
    i = list("Hello" = 3, "World" = 8),
    j = list("Foo" = 2:3, "Bar" = 4:5))
expect_snapshot_print(print_html(tab), "group_tt-html_tutorial_01.html")



# Issue #258: group_tt indices with 0 and duplicates
tab1 <- tt(head(iris)) |> group_tt(i = list("hello" = 1))
t1 <- expect_table(tab1)
expect_snapshot_print(t1[["latex"]], "group_tt-issue258_01_latex.tex")
expect_snapshot_print(t1[["typst"]], "group_tt-issue258_01_typst.typ")
expect_snapshot_print(t1[["markdown"]], "group_tt-issue258_01_markdown.md")
expect_snapshot_print(t1[["html"]], "group_tt-issue258_01_html.html")

tab2 <- tt(head(iris)) |> group_tt(i = list("hello" = 2, "world" = 2))
t2 <- expect_table(tab2)
expect_snapshot_print(t2[["latex"]], "group_tt-issue258_02_latex.tex")
expect_snapshot_print(t2[["typst"]], "group_tt-issue258_02_typst.typ")
expect_snapshot_print(t2[["markdown"]], "group_tt-issue258_02_markdown.md")
expect_snapshot_print(t2[["html"]], "group_tt-issue258_02_html.html")

# Issue #362: group_tt with duplicate column group labels in latex
tab <- data.frame(
  sub1 = 1,
  sub2 = 2,
  sub1 = 3,
  sub2 = 4,
  check.names = FALSE
) |>
  tt() |>
  group_tt(j = list(a = 1:2, a = 3:4))
tab@output <- "latex"
expect_snapshot_print(tab, "group_tt-issue362_duplicate_colum_labels.tex")

# Issue #413: Automatic row groups fail if column is a factor
df <- mtcars |> head(10)
df <- df[order(df$am), ]
df$am <- factor(df$am)
tab <- tt(df) |> group_tt(i = df$am)
expect_inherits(tab, "tinytable")
expect_inherits(save_tt(tab, "markdown"), "character")


# delimiters in column names
# No delimiter
tab <- tt(mtcars |> head())
tab@output <- "markdown"
expect_snapshot_print(tab, "group_tt_delim-no-delim.md")
tab <- tt(data.frame(A_id = 1, A_a1 = 2, A_a2 = "3", B_b1 = 4, B_b2 = 5, B_C = 6)) |>
  group_tt(j = "_")
tab@output <- "markdown"
expect_snapshot_print(tab, "group_tt_delim-all-delim.md")
tab <- tt(data.frame(id = 1, A_a1 = 2, A_a2 = "3", B_b1 = 4, B_b2 = 5, C = 6)) |>
  group_tt(j = "_")
tab@output <- "markdown"
expect_snapshot_print(tab, "group_tt_delim-some-delim.md")
tab <- tt(data.frame(id = 1, Axa1 = 2, Axa2 = "3", Bxb1 = 4, Bxb2 = 5, C = 6)) |>
  group_tt(j = "x")
tab@output <- "markdown"
expect_snapshot_print(tab, "group_tt_delim-x-delim.md")


# Issue #466: group_tt without delimiter returns a valid table
tab <- tt(data.frame(Aid = 1, Aa1 = 2, Aa2 = "3", Bb1 = 4, Bb2 = 5, BC = 5)) |>
  group_tt(j = "_")
expect_inherits(tab, "tinytable")


# Issue #471: `style_tt()` group
set.seed(48103)
dat <- data.frame(
  label = c("a", "a", "a", "b", "b", "c", "a", "a"),
  x1 = rnorm(8),
  x2 = rnorm(8)
)
expect_warning(
  tt(dat[, 2:3]) |>
    style_tt(i = "groupi", color = "white", background = "black") |>
    group_tt(i = dat$label),
  pattern = "style.*before"
)

expect_false(ignore(expect_warning)(
  tt(dat[, 2:3]) |>
    group_tt(i = dat$label) |>
    style_tt(i = "groupi", color = "white", background = "black")
))

tab <- tt(dat[, 2:3]) |>
  group_tt(i = dat$label, indent = 0) |>
  style_tt(i = "groupi", color = "white", background = "black", align = "c")
tab@output <- "latex"
expect_snapshot_print(tab, label = "group_tt_style_tt_group.tex")


# Issue #165: Column labels not centered, last row not styled because of row group offset
k = mtcars[1:5, 1:5]
k[1, 1] <- pi
tab <- tt(k) |>
  format_tt(num_fmt = "decimal", num_zero = TRUE, digits = 3) |>
  style_tt(i = 1:3, j = 1, bold = TRUE) |>
  style_tt(j = 2, italic = TRUE) |>
  group_tt(i = list("hello world" = 2), j = list("foo" = 1:2, "bar" = 3:4))
expect_snapshot_print(
  print_html(tab),
  "group_tt-issue165_html_centering_style.html")


# Issue #165: group_tt insert extra row at the bottom
k <- mtcars[1:4, 1:4]
tab <- tt(k) |>
  format_tt(num_fmt = "decimal", num_zero = TRUE, digits = 3) |>
  style_tt(i = 1:3, j = 1, bold = TRUE) |>
  style_tt(j = 2, italic = TRUE) |>
  group_tt(
    i = list("hello world" = 2),
    j = list("foo" = 1:2, "bar" = 3:4)
  )
t <- expect_table(tab)
expect_snapshot_print(t[["latex"]], "group_tt-issue165_extra_row.tex")
expect_snapshot_print(t[["typst"]], "group_tt-issue165_extra_row.typ")
expect_snapshot_print(t[["html"]], "group_tt-issue165_extra_row.html")
exit_file("TODO: broken markdown output")
expect_snapshot_print(t[["markdown"]], "group_tt-issue165_extra_row.md")
