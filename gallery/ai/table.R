library("ggplot2")
library("scales")
library("tinytable")
source("plot.R")

# Read stock data from CSV file
stock_data = read.csv("data_companies.csv")
ticker_data = read.csv("data_ticker.csv")
ticker_52week_data = read.csv("data_ticker_52.csv")

# Get the latest date from 52-week data
ticker_52week_data$date = as.Date(ticker_52week_data$date)
latest_date = max(ticker_52week_data$date)
latest_date_col = format(latest_date, "%d %b %Y")
latest_date_fmt = format(latest_date, "%B %d, %Y")


# Join original stock data with generated table data
final_df = merge(stock_data, ticker_data, by = "Ticker")

# Add logo path, remove Description column, and modify column names
final_df$logo = paste0("Logos/", final_df$Ticker, ".png")
final_df$Description = NULL
final_df$Price_Trend = final_df$Ticker
final_df$Return_Trend = final_df$Ticker
final_df$Volume_Trend = final_df$Ticker
final_df$Company.Name = toupper(final_df$Company.Name)
final_df$Ticker_Symbol = style_vector(final_df$Ticker, color = "grey")

# Update the company name for a specific ticker
final_df[final_df$Ticker == "IBM", "Company.Name"] = "IBM CORPORATION"

# Stack Company Name and Ticker in a single cell using HTML
final_df$Company = sprintf(
  "<span style='font-size:12px;font-weight:bold'>%s</span><br><span style='font-size:11px;font-weight:bold'>%s</span>",
  final_df$Company.Name, final_df$Ticker_Symbol
)

# Select relevant columns for the table
req_df = final_df[, c("logo", "Company", "Currency", "current_price",
                      "pct_chng_prev_price", "Return_Trend", "high_52_week", "low_52_week",
                      "Price_Trend", "current_vol", "pct_chng_prev_vol", "Volume_Trend", "Ticker")]

# Adjust currency label for formatting
req_df$Currency = ifelse(req_df$Currency == "GBp", "pound", "dollar")

# Format the percentage change columns with up/down triangles
req_df$pct_chng_prev_price = sapply(req_df$pct_chng_prev_price, function(x) {
  formatted_value = sprintf("%.2f%%", abs(x) * 100)
  if (x > 0) {
    paste0("<span style='color:#27b7ac'>&#9650;</span> ", formatted_value)
  } else {
    paste0("<span style='color:#d82466'>&#9660;</span> -", formatted_value)
  }
})

req_df$pct_chng_prev_vol = sapply(req_df$pct_chng_prev_vol, function(x) {
  formatted_value = sprintf("%.2f%%", abs(x) * 100)
  if (x > 0) {
    paste0("<span style='color:#27b7ac'>&#9650;</span> ", formatted_value)
  } else {
    paste0("<span style='color:#d82466'>&#9660;</span> -", formatted_value)
  }
})

# Format currency columns
req_df$current_price = mapply(function(price, curr) {
  scales::label_currency(prefix = ifelse(curr == "dollar", "$", "\u00A3"))(price)
}, req_df$current_price, req_df$Currency)

req_df$high_52_week = mapply(function(price, curr) {
  scales::label_currency(prefix = ifelse(curr == "dollar", "$", "\u00A3"))(price)
}, req_df$high_52_week, req_df$Currency)

req_df$low_52_week = mapply(function(price, curr) {
  scales::label_currency(prefix = ifelse(curr == "dollar", "$", "\u00A3"))(price)
}, req_df$low_52_week, req_df$Currency)

# Format volume column
req_df$current_vol = sprintf("%.2fM", req_df$current_vol / 1000000)

# Remove Currency and Ticker columns (no longer needed)
req_df = req_df[, !colnames(req_df) %in% c("Currency", "Ticker")]

# Calculate table width (use fixed width for cross-platform compatibility)
table_width = 1496

# Set up column names with HTML formatting
req_df = setNames(req_df, c(
  "",
  "Company",
  paste0("Price<br>", latest_date_col),
  "Price<br>% Change",
  "Price<br>% Change",
  "52-Week High",
  "52-Week Low",
  "Price Trend",
  paste0("Volume<br>", latest_date_col),
  "Volume<br>% Change",
  "Volume<br>Trend"
))

# Relative column width
w = c(2.5, 2, 1.3, 1, 2, 1, 1, 2, 1.3, 1, 2)

# Create caption and notes
title = style_vector("Top AI Stock Performance", smallcap = TRUE, fontsize = 1.4)
subtitle = style_vector("This table summarises the performance of top 14 AI Stocks by Market Cap (May 2024)</p>", italic = TRUE)
caption = paste(title, subtitle, sep = "<br>")
notes = paste0("Original table designed by [Arnav Chauhan.](https://arnavchauhan.quarto.pub/top-ai-stock-analysis-tablecontest2024/). Data Last Updated: ", latest_date_fmt)

# Generates a table summarizing top AI stock performance
table_final = tt(req_df, caption = caption, notes = notes, width = w) |>
  theme_empty() |>
  theme_striped() |>
  format_tt("notes", markdown = TRUE) |>
  # Add logos
  plot_tt(
    j = 1,
    images = final_df$logo,
    height = 2) |>
  # Add weekly return trend plots
  plot_tt(
    j = 5,
    fun = plot_weekly_return_trend,
    data = as.list(final_df$Ticker),
    height = 4,
    height_plot = 500,
    width_plot = 900) |>
  # Add price trend plots
  plot_tt(
    j = 8,
    fun = plot_price_trend,
    data = as.list(final_df$Ticker),
    height = 4,
    height_plot = 500,
    width_plot = 900) |>
  # Add volume trend plots
  plot_tt(
    j = 11,
    fun = plot_volume_trend,
    data = as.list(final_df$Ticker),
    height = 4,
    height_plot = 500,
    width_plot = 900) |>
  # Alignment
  style_tt(align = "c", alignv = "m") |>
  style_tt(j = 1, align = "r") |>
  style_tt(j = 2, align = "l") |>
  # Column header styling
  style_tt(i = 0, bold = TRUE, color = "white", background = "#10100F", alignv = "m") |>
  # Link in notes
  format_tt("notes", markdown = TRUE)

# View the table
print(table_final, "html")

# save_tt(table_final, "ai_stock_table.html", overwrite = TRUE)
