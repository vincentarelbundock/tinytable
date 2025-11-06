source("helpers.R")
options(tinytable_html_engine = "tabulator")
options(tinytable_tt_theme = theme_html)

# Basic tabulator table
dat <- head(iris)
tab <- tt(dat)
out <- save_tt(tab, "html")
expect_true(is.character(out))
expect_true(grepl("tabulator", out))
expect_true(grepl("Sepal.Length", out))

# Preserve special characters in column names
special_dat <- data.frame(`hello\\?` = 1, check.names = FALSE)
tab <- tt(special_dat)
out <- save_tt(tab, "html")
expect_true(grepl('"hello_\\?"', out))
expect_false(grepl('"hello__"', out))

# Data with multiple types
dat <- data.frame(
  city = c("Montréal", "Toronto", "Vancouver"),
  salary = c(14002.22, 201399.11, 80188.38),
  random = c(1.43402, 201.399, 0.134588),
  date = as.Date(sample(1:1000, 3), origin = "1970-01-01"),
  best = c(TRUE, FALSE, FALSE)
)

tab <- tt(dat)
out <- save_tt(tab, "html")
expect_true(is.character(out))
expect_true(grepl("Montréal", out))
expect_true(grepl("true", out, ignore.case = TRUE))

# Pagination and filtering
tab <- tt(iris) |> theme_html(
  tabulator_pagination = c(5, 10, 50),
  tabulator_search = "top"
)
out <- save_tt(tab, "html")
expect_true(grepl("pagination", out))
expect_true(grepl("search", out))
expect_true(grepl("paginationSize.*5", out))

# Format numeric columns
tab <- tt(dat) |>
  format_tt(j = "salary", digits = 2, num_mark_big = ",") |>
  format_tt(j = "random", digits = 4)
out <- save_tt(tab, "html")
expect_true(grepl("decimal", out))
expect_true(grepl("salary.*decimal", out))

# Format date columns
tab <- tt(dat) |> format_tt(j = "date", date = "M/d/yyyy")
out <- save_tt(tab, "html")
expect_true(grepl("M/d/yyyy", out))
expect_true(grepl("datetime", out))

# Simple data table for remaining tests
simple_dat <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  value = c(100, 200, 300)
)

tab <- tt(simple_dat)
out <- save_tt(tab, "html")
expect_true(grepl("Alice", out))

# Style alignment
tab <- tt(simple_dat) |> style_tt(align = "r")
out <- save_tt(tab, "html")
expect_true(grepl("right", out))

# Custom CSS rules
css_rule <- "
$TINYTABLE_ID .tabulator-header .tabulator-col {
  background-color: black;
  color: white;
}
"
tab <- tt(simple_dat) |> theme_html(tabulator_css_rule = css_rule)
out <- save_tt(tab, "html")
expect_true(grepl("background-color.*black", out))
expect_true(grepl("color.*white", out))

# Custom columns
dat_custom <- data.frame(
  city = c("Toronto", "Montreal", "Vancouver"),
  salary = c(75000, 68000, 82000),
  best = c(FALSE, TRUE, FALSE)
)

custom_columns <- '[
  {
    "title": "City",
    "field": "city"
  },
  {
    "title": "Best city",
    "field": "best",
    "formatter": "tickCross"
  },
  {
    "title": "Salary",
    "field": "salary",
    "formatter": "money",
    "formatterParams": {"precision": 0, "symbol": "$"}
  }
]'

tab <- tt(dat_custom) |> theme_html(tabulator_columns = custom_columns)
out <- save_tt(tab, "html")
expect_true(grepl("tickCross", out))
expect_true(grepl("money", out))
expect_true(grepl("Best city", out))

# Custom options
opts <- "
  layout: 'fitColumns',
  height: '200px'
"

tab <- tt(dat_custom) |> theme_html(tabulator_options = opts)
out <- save_tt(tab, "html")
expect_true(grepl("fitColumns", out))
expect_true(grepl("200px", out))

# Reset global option
options(tinytable_html_engine = NULL)
