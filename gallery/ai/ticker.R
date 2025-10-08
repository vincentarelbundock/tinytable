library("quantmod")

# Read stock data from CSV file
stock_data = read.csv("data_companies.csv", stringsAsFactors = FALSE)

# Function to get table data for a given company ticker
get_table_data = function(company_ticker) {
  # Retrieve stock data for the last 52 weeks
  stock_data_52week_xts = quantmod::getSymbols.yahoo(company_ticker, from = Sys.Date() - (52 * 7), to = Sys.Date(), auto.assign = FALSE)

  # Convert xts object to data frame
  stock_data_52week = data.frame(date = index(stock_data_52week_xts), coredata(stock_data_52week_xts))
  stock_data_52week = na.omit(stock_data_52week)

  # Select relevant columns and rename them
  cols_to_keep = c("date", paste0(company_ticker, ".Adjusted"), paste0(company_ticker, ".Volume"))
  stock_data_52week = stock_data_52week[, cols_to_keep]
  colnames(stock_data_52week)[2] = "Adj_Close"
  colnames(stock_data_52week)[3] = "Volume"

  # Calculate various metrics based on stock data
  stock_data_52week = stock_data_52week[order(stock_data_52week$date), ]
  prev_day_price = tail(stock_data_52week, n = 2)[1, "Adj_Close"]
  current_price = tail(stock_data_52week, n = 1)[1, "Adj_Close"]
  pct_chng_prev_price = ((current_price - prev_day_price) / prev_day_price)
  high_52week = max(stock_data_52week$Adj_Close, na.rm = TRUE)
  low_52week = min(stock_data_52week$Adj_Close, na.rm = TRUE)
  current_vol = tail(stock_data_52week, n = 1)[1, "Volume"]
  prev_day_vol = tail(stock_data_52week, n = 2)[1, "Volume"]
  pct_chng_prev_vol = (current_vol - prev_day_vol) / prev_day_vol

  # Create table data as a data frame
  table_data = data.frame(
    Ticker = company_ticker,
    current_price = current_price,
    pct_chng_prev_price = pct_chng_prev_price,
    high_52_week = high_52week,
    low_52_week = low_52week,
    current_vol = current_vol,
    pct_chng_prev_vol = pct_chng_prev_vol,
    stringsAsFactors = FALSE
  )

  return(table_data)
}
result = do.call(rbind, lapply(stock_data$Ticker, function(x) get_table_data(x)))

write.csv(result, "data_ticker.csv", row.names = FALSE)

# Function to get 52-week historical data for a given company ticker
get_52week_data = function(company_ticker) {
  # Retrieve stock data for the last 52 weeks
  stock_data_52week_xts = quantmod::getSymbols.yahoo(company_ticker, from = Sys.Date() - (52 * 7), to = Sys.Date(), auto.assign = FALSE)

  # Convert xts object to data frame
  stock_data_52week = data.frame(date = index(stock_data_52week_xts), coredata(stock_data_52week_xts))
  stock_data_52week = na.omit(stock_data_52week)

  # Select relevant columns and rename them
  cols_to_keep = c("date", paste0(company_ticker, ".Adjusted"), paste0(company_ticker, ".Volume"))
  stock_data_52week = stock_data_52week[, cols_to_keep]
  colnames(stock_data_52week) = c("date", "Adj_Close", "Volume")

  # Add Ticker column
  stock_data_52week$Ticker = company_ticker

  return(stock_data_52week)
}

# Download 52-week data for all tickers
result_52week = do.call(rbind, lapply(stock_data$Ticker, function(x) get_52week_data(x)))

write.csv(result_52week, "data_ticker_52.csv", row.names = FALSE)
