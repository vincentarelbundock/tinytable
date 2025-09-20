source("helpers.R")

# date
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


# markdown
input <- c(
  "This is **bold** text",
  "This is *italic* text",
  "This is `code` text")
output <- c(
  "This is <strong>bold</strong> text",
  "This is <em>italic</em> text",
  "This is <code>code</code> text")
expect_equivalent(format_vector(input, markdown = TRUE), output)
