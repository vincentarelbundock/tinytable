# Load 52-week data
ticker_52week_data = read.csv("data_ticker_52.csv", stringsAsFactors = FALSE)
ticker_52week_data$date = as.Date(ticker_52week_data$date)

# Function to plot price trend for a given company ticker
plot_price_trend = function(company_ticker, ...) {
  # Filter data for the specific ticker
  price_plot_data_df = ticker_52week_data[ticker_52week_data$Ticker == company_ticker, c("date", "Adj_Close")]

  # Calculate start and end prices
  price_plot_data_df = price_plot_data_df[order(price_plot_data_df$date), ]
  start_price = head(price_plot_data_df, n = 1)[1, "Adj_Close"]
  end_price = tail(price_plot_data_df, n = 1)[1, "Adj_Close"]

  # Find highest and lowest points
  highest_point = price_plot_data_df[price_plot_data_df$Adj_Close == max(price_plot_data_df$Adj_Close), ]
  lowest_point = price_plot_data_df[price_plot_data_df$Adj_Close == min(price_plot_data_df$Adj_Close), ]

  # Plot price trend
  ggplot(price_plot_data_df, aes(x = date, y = Adj_Close)) +
    geom_area(fill = ifelse(end_price > start_price, "#27b7ac", "#d82466"), alpha = 0.3) +
    geom_line(color = ifelse(end_price > start_price, "#27b7ac", "#d82466")) +
    geom_point(data = highest_point, aes(x = date, y = Adj_Close), color = "#27b7ac", size = 5) +
    geom_point(data = lowest_point, aes(x = date, y = Adj_Close), color = "#d82466", size = 5) +
    expand_limits(y = 0) +
    theme_void()
}

# Function to plot weekly return trend for a given company ticker
plot_weekly_return_trend = function(company_ticker, ...) {
  # Filter data for the specific ticker
  return_plot_data_df = ticker_52week_data[ticker_52week_data$Ticker == company_ticker, c("date", "Adj_Close")]

  # Filter data for Fridays only
  return_plot_data_df = return_plot_data_df[weekdays(return_plot_data_df$date) == "Friday", ]

  # Calculate percentage change in price (lag equivalent using indexing)
  n = nrow(return_plot_data_df)
  return_plot_data_df$pct_change_in_price = c(NA, (return_plot_data_df$Adj_Close[2:n] / return_plot_data_df$Adj_Close[1:(n-1)]) - 1)

  # Assign color based on price change direction
  return_plot_data_df$color = ifelse(return_plot_data_df$pct_change_in_price > 0, "#27b7ac", "#d82466")

  return_plot_data_df$date = as.factor(return_plot_data_df$date)

  # Plot weekly return trend
  ggplot(return_plot_data_df, aes(x = date, y = pct_change_in_price, fill = color)) +
    geom_col(width = 0.9) +
    scale_fill_identity() +
    scale_x_discrete(expand = c(0, 0)) +
    theme_void()
}

# Function to plot volume trend for a given company ticker
plot_volume_trend = function(company_ticker, ...) {
  # Filter data for the specific ticker
  volume_plot_data_df = ticker_52week_data[ticker_52week_data$Ticker == company_ticker, c("date", "Volume")]

  # Calculate start and end volume
  volume_plot_data_df = volume_plot_data_df[order(volume_plot_data_df$date), ]
  start_vol = head(volume_plot_data_df, n = 1)[1, "Volume"]
  end_vol = tail(volume_plot_data_df, n = 1)[1, "Volume"]

  # Find highest and lowest volume
  highest_vol = volume_plot_data_df[volume_plot_data_df$Volume == max(volume_plot_data_df$Volume), ]
  lowest_vol = volume_plot_data_df[volume_plot_data_df$Volume == min(volume_plot_data_df$Volume), ]

  # Plot volume trend
  ggplot(volume_plot_data_df, aes(x = date, y = Volume)) +
    geom_area(fill = ifelse(end_vol > start_vol, "#27b7ac", "#d82466"), alpha = 0.3) +
    geom_line(color = ifelse(end_vol > start_vol, "#27b7ac", "#d82466")) +
    geom_point(data = highest_vol, aes(x = date, y = Volume), color = "#27b7ac", size = 5) +
    geom_point(data = lowest_vol, aes(x = date, y = Volume), color = "#d82466", size = 5) +
    expand_limits(y = 0) +
    theme_void()
}
