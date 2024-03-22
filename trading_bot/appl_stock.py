import yfinance as yf
import pandas as pd

# Author: Carson Wolber, Date: 2-28-2024
# If we want to access data for a specific stock, such as Apple (AAPL), we do this:

# Set the start and end dates for the time range you're interested in
start = '2016-02-24'  
end = '2024-02-24'  

# Download data for Apple (AAPL) using yfinance for our specified timeframe
data = yf.download(tickers='SOY', start=start, end=end)

# yfinance will give us, for each day in our period, adjusted close (adjusted closing price), 
# closing price, high, low, open (starting price), and volume (number of shares traded).
# This data is already structured in a way that's typically easy to work with for a single stock.

# Rename the index to 'date' and columns to lower case for consistency
data.index.name = 'date'
data.columns = data.columns.str.lower()

aapl = yf.download("AAPL", period="10y", interval="1d")

aapl.index.name = 'date'
aapl.columns = data.columns.str.lower()

# Export this data to a CSV file
aapl.to_csv("apple_stock_data.csv")


