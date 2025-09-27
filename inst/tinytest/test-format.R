source("helpers.R")
using("tinysnapshot")

requiet("Rdatasets")

# bug discovered in modelsummary
z <- factor(c("a", "b", NA))
a <- format_tt(z, replace = "-")
b <- c("a", "b", "-")
expect_equivalent(a, b)

# numeric vector input
a <- c(98938272783457, 7288839482, 29111727, 93945)
b <- format_tt(a, num_suffix = TRUE, digits = 2)
expect_equivalent(b, c("99T", "7.3B", "29M", "94K"))

# num_suffix (vignette)
options(tinytable_print_output = "markdown")

dat <- data.frame(
  a = c("Burger", "Halloumi", "Tofu", "Beans"),
  b = c(1.43202, 201.399, 0.146188, 0.0031),
  c = c(98938272783457, 7288839482, 29111727, 93945)
)
tab <- tt(dat) |>
  format_tt(j = "a", sprintf = "Food: %s") |>
  format_tt(j = 2, digits = 1) |>
  format_tt(j = "c", digits = 2, num_suffix = TRUE)
expect_snapshot_print(tab, label = "format_tt-num_suffix_vignette")

set.seed(1024)
dat <- data.frame(
  w = c(143002.2092, 201399.181, 100188.3883),
  x = c(1.43402, 201.399, 0.134588),
  y = as.Date(sample(1:1000, 3), origin = "1970-01-01"),
  z = c(TRUE, TRUE, FALSE)
)
tab <- tt(dat, digits = 2)
expect_snapshot_print(tab, label = "format_tt-vignette_digits")

tab <- tt(dat) |>
  format_tt(
    j = 2:4,
    digits = 1,
    date = "%B %d %Y"
  ) |>
  format_tt(
    j = 1,
    digits = 2,
    num_mark_big = " ",
    num_mark_dec = ",",
    num_fmt = "decimal"
  )
expect_snapshot_print(tab, label = "format_tt-vignette_misc")

expect_snapshot_print(
  format_tt(dat, digits = 1, num_suffix = TRUE),
  label = "format_tt-dataframe"
)

# custom formatting
x <- mtcars[1:3, 1:3]
tab <- tt(x) |> format_tt(fn = function(x) paste("Ya", x))
expect_snapshot_print(tab, "format_tt-fn")

# Issue #142
k <- data.frame(x = c(0.000123456789, 12.4356789))
tab <- tt(k, digits = 2)
expect_snapshot_print(tab, "format_tt-issue142_01")
tab <- tt(k) |> format_tt(digits = 2, num_fmt = "significant_cell")
expect_snapshot_print(tab, "format_tt-issue142_02")

# Issue #147: format_tt(escape = TRUE) zaps previous formatting
options(tinytable_print_output = "latex")
x <- data.frame(num = c(pi, pi), char = c("10$", "blah_blah"))
tab <- tt(x) |>
  format_tt(i = 1, j = 1, digits = 2) |>
  format_tt(i = 1, j = 1, digits = 3) # overwrite
expect_snapshot_print(tab, "format_tt-issue147_01.tex")
tab <- tt(x) |>
  format_tt(i = 1, j = 1, digits = 2) |>
  format_tt(i = 2, j = 1, digits = 3) # different cell
expect_snapshot_print(tab, "format_tt-issue147_02.tex")
tab <- tt(x) |>
  format_tt(i = 1, j = 1, digits = 2) |>
  format_tt(i = 2, j = 1, digits = 3) |>
  format_tt(escape = TRUE) # do not zap
expect_snapshot_print(tab, "format_tt-issue147_03.tex")
options(tinytable_print_output = NULL)

# Issue #149: num_mark_big requires digits
x <- data.frame(x = c(pi * 1e6, pi * 1e9))
tab <- format_tt(tt(x), digits = 1, num_mark_big = " ")
expect_snapshot_print(tab, "format_tt-issue149")
# expect_error(format_tt(tt(x), num_mark_big = " "))

x <- data.frame(x = 1:5, y = c(pi, NA, NaN, -Inf, Inf))
dict <- list("-" = c(NA, NaN), "Tiny" = -Inf, "Huge" = Inf)
tab <- tt(x) |>
  format_tt(replace = dict) |>
  save_tt("dataframe")
expect_equivalent(tab[[2]], c("y", "3.141593", "-", "-", "Tiny", "Huge"))

# Issue #256: big mark for integers
tab <- data.frame(x = c(1332037, 1299128, 805058, 206840, 698511)) |>
  tt() |>
  format_tt(num_mark_big = " ", digits = 0, num_fmt = "decimal") |>
  save_tt("dataframe")
expect_equivalent(
  tab[[1]],
  c("x", "1 332 037", "1 299 128", "805 058", "206 840", "698 511")
)

x <- data.frame(x = pi, y = NA)
options(tinytable_tt_digits = 2)
tab <- tt(x) |> save_tt("dataframe")
expect_equivalent(tab[[2]], c("y", ""))

options(tinytable_format_replace = "zzz")
tab <- tt(x) |> save_tt("dataframe")
expect_equivalent(tab[[2]], c("y", "zzz"))

options(tinytable_format_replace = FALSE)
tab <- tt(x) |> save_tt("dataframe")
expect_equivalent(tab[[2]], c("y", "NA"))

options(tinytable_print_output = NULL)
options(tinytable_tt_digits = NULL)
options(tinytable_format_replace = NULL)

# Issue #263: NA processing with Quarto doesn't work
x <- data.frame(
  Points = c(40, 0, 10, 10, 10, 10, 10, 20, 110),
  Assignment = c(
    "Analytical Assignments",
    "Team Project Part 0 - County Selection",
    "Team Project Part 1 - Summary Background",
    "Team Project Part 2 - SWOT Analysis",
    "Team Project Part 3 - Strategic Direction/Action Plan",
    "Team Project Part 4 - Evaluation Framework",
    "Team Project Part 5 - Plan Presentation",
    "Class participation",
    "Total"
  ),
  Percent = c("36%", "0%", "9%", "9%", "9%", "9%", "9%", "18%", NA)
)
x <- tt(x) |>
  format_tt(replace = "!", quarto = TRUE) |>
  save_tt("latex")
expect_true(grepl("IQ==", x))

# Website scaling example
thumbdrives <- data.frame(
  date_lookup = as.Date(
    c("2024-01-15", "2024-01-18", "2024-01-14", "2024-01-16")
  ),
  price = c(18.49, 19.99, 24.99, 24.99),
  price_rank = c(1, 2, 3, 3),
  memory = c(16e9, 12e9, 10e9, 8e9),
  speed_benchmark = c(0.6, 0.73, 0.82, 0.99)
)
tab <- tt(thumbdrives) |>
  format_tt(j = 1, fn = scales::date_format("%B %d %Y")) |>
  format_tt(j = 2, fn = scales::label_currency()) |>
  format_tt(j = 3, fn = scales::label_ordinal()) |>
  format_tt(j = 4, fn = scales::label_bytes()) |>
  format_tt(j = 5, fn = scales::label_percent()) |>
  format_tt(escape = TRUE) |>
  save_tt("dataframe")
expect_true("January 15 2024" %in% tab[[1]])
expect_false("2024-01-15" %in% tab[[1]])
expect_true("$18.49" %in% tab[[2]])
expect_true("16 GB" %in% tab[[4]])
expect_true("99%" %in% tab[[5]])

# Issue #409: both NA and NaN should be replaced
options(tinytable_format_replace = NULL)
tab <- data.frame(x = c(1, NA, NaN, Inf))
tab0 <- tt(tab) |> save_tt("dataframe")
tab1 <- tt(tab) |>
  format_tt() |>
  save_tt("dataframe")
tab2 <- tt(tab) |>
  format_tt(replace = TRUE) |>
  save_tt("dataframe")
expect_equivalent(tab0[[1]], c("x", "1", "NA", "NaN", "Inf"))
expect_equivalent(tab1[[1]], c("x", "1", "NA", "NaN", "Inf"))
expect_equivalent(tab2[[1]], c("x", "1", "", "", "Inf"))

# bug: duplicated columns with markdown html
dat <- data.frame(
  markdown = c(
    "This is _italic_ text.",
    "This sentence ends with a superscript.^2^"
  )
)
tab <- tt(dat) |>
  format_tt(j = 1, markdown = TRUE) |>
  style_tt(j = 1, align = "c")
expect_snapshot_print(print_html(tab), "format_tt-vignette_html_markdown.html")

# boolean formatting
tab <- tt(data.frame(a = c(TRUE, FALSE, TRUE, NA)))
tab1 <- format_tt(tab, bool = tolower) |> save_tt("dataframe")
tab2 <- format_tt(tab, j = 1, bool = tolower) |> save_tt("dataframe")
expect_equivalent(tab1$a, tab2$a)


# Issue #392
pen <- rddata("penguins", "palmerpenguins")[1:3, c("species", "body_mass_g")]
expect_error(format_tt(tt(pen), num_mark_big = ","), "requires.*digits")
expect_error(format_tt(1234, num_mark_big = ","), "requires.*digits")


# Component-specific formatting Issue #418
dat <- data.frame(x = 1:2, y = 3:4)
tab <- tt(dat, caption = "Test Caption", notes = "Test note")

tab1 <- format_tt(tab, i = "colnames", fn = function(x) paste0("Col_", x))
expect_true(grepl("Col_x", save_tt(tab1, "markdown")))
expect_false(grepl("Col_2", save_tt(tab1, "markdown")))

tab1 <- format_tt(tab, i = "caption", fn = function(x) paste0("Col_", x))
expect_true(grepl("Col_Test", save_tt(tab1, "markdown")))
expect_false(grepl("Col_2", save_tt(tab1, "markdown")))

tab1 <- format_tt(tab, i = "notes", fn = function(x) paste0("Col_", x))
expect_true(grepl("| Col_Test", save_tt(tab1, "markdown"), fixed = TRUE))
expect_false(grepl("Col_2", save_tt(tab1, "markdown")))

# Test formatting only notes
tab3 <- format_tt(tab, i = "notes", fn = function(x) paste0("Note: ", x))
expect_true(grepl("| Note: |", save_tt(tab3, "markdown"), fixed = TRUE))
expect_true(grepl("| Test  |", save_tt(tab3, "markdown"), fixed = TRUE))

# Test multiple components at once
tab4 <- format_tt(tab, i = c("colnames", "caption"), fn = function(x) {
  paste0("Prefix_", x)
})
expect_true(grepl("Prefix_x", save_tt(tab4, "markdown"), fixed = TRUE))
expect_true(grepl(
  "Prefix_Test Caption",
  save_tt(tab4, "markdown"),
  fixed = TRUE
))


# groupi vs. ~groupi
tab = head(iris) |>
  tt() |>
  group_tt(i = list("Hello" = 1, "World" = 3)) |>
  format_tt("~groupi", j = 1, sprintf = "--%s--") |>
  save_tt("dataframe")
expect_true("Hello" %in% tab[[1]])
expect_true("--5.1--" %in% tab[[1]])
expect_false("--Hello--" %in% tab[[1]])
expect_false("5.1" %in% tab[[1]])

tab = head(iris) |>
  tt() |>
  group_tt(i = list("Hello" = 1, "World" = 3)) |>
  format_tt("groupi", j = 1, sprintf = "--%s--") |>
  save_tt("dataframe")
expect_true("--Hello--" %in% tab[[1]])
expect_true("5.1" %in% tab[[1]])
expect_false("Hello" %in% tab[[1]])
expect_false("--5.1--" %in% tab[[1]])


# Vignette with multiple components
options(tinytable_print_output = "latex")
set.seed(48103)
tab <- data.frame(
  "A_B" = rnorm(5),
  "B_C" = rnorm(5),
  "C_D" = rnorm(5)
)
tab <- tt(tab, digits = 2, notes = "_Source_: Simulated data.") |>
  group_tt(i = list("Down" = 1, "Up" = 3)) |>
  format_tt("colnames", fn = \(x) sub("_", " / ", x)) |>
  format_tt("notes", markdown = TRUE) |>
  format_tt("groupi", replace = list("↓" = "Down", "↑" = "Up"))

expect_snapshot_print(
  tab,
  label = "format_tt-format_components_vignette_01.tex"
)
options(tinytable_print_output = NULL)


# format group_tt(i)
dat <- data.frame(x = 1:4, y = letters[1:4])
tab <- tt(dat) |>
  group_tt(i = list("Group A" = 2, "Group B" = 4)) |>
  format_tt(i = c(2, 5), j = 1, sprintf = "_%s_") |>
  format_tt(i = 1:3, j = 1, sprintf = "*%s*")
expect_snapshot_print(tab, label = "format_tt-group_tt_01.md")


dat1 <- data.frame(
  values = c(123.456, 789.012, 345.678, 901.234),
  labels = c("A", "B", "C", "D")
)
tab1 <- tt(dat1) |>
  group_tt(i = list("First Group" = 2, "Second Group" = 4)) |>
  format_tt(j = 1, sprintf = "((%s))")
expect_snapshot_print(tab1, label = "format_tt-group_tt_02.md")


# Line breaks using linebreak argument
d <- data.frame(Text = "First line<br>Second line")
tab <- tt(d) |> format_tt(linebreak = "<br>")
t <- expect_table(tab, formats = c("html", "latex", "typst"))
expect_true(grepl("First line<br>Second line", t[["html"]]))
expect_true(grepl("First line\\\\Second line", t[["latex"]], fixed = TRUE))
expect_true(grepl("First line \\ Second line", t[["typst"]], fixed = TRUE))

# Date formatting tests
dates <- as.Date(c("2023-01-01", "2023-12-31"))
formatted_dates <- format_vector(dates, date = "%Y")
expect_equivalent(formatted_dates, c("2023", "2023"))
formatted_dates_full <- format_vector(dates, date = "%B %d, %Y")
expect_equivalent(formatted_dates_full, c("January 01, 2023", "December 31, 2023"))
formatted_vector <- format_vector(dates, date = "%Y")
expect_equivalent(formatted_vector, c("2023", "2023"))
d <- data.frame(dates = dates)
tab <- tt(d) |> format_tt(j = 1, date = "%Y")
formatted_table <- save_tt(tab, "dataframe")
expect_equivalent(formatted_table[[1]], c("dates", "2023", "2023"))


# Conditional formatting
dat <- data.frame(a = c("*Hello*", "**World**"))
cap <- "HTML render 1st row. LaTeX render 2nd row."
tab <- tt(dat, caption = cap) |>
  format_tt(i = 1, j = 1, markdown = TRUE, output = "html") |>
  format_tt(i = 2, j = 1, markdown = TRUE, output = "latex")
t <- expect_table(tab, formats = c("html", "latex"))
expect_snapshot_print(t[["html"]], "format_tt-conditional_output.html")
expect_snapshot_print(t[["latex"]], "format_tt-conditional_output.tex")
