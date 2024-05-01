import yfinance as yf
import pandas as pd
import sys

# Author: Carson Wolber, Date: 2-28-2024
# If we want to access data for a specific stock, such as Apple (AAPL), we do this:

# Use the ticker provided in the command line argument
ticker = sys.argv[1]

try:
  print ("reached python" )
  data = yf.download(tickers=ticker, period="10y", interval="1d")
  data.index.name = 'date'
  data.columns = data.columns.str.lower()

  # Save the downloaded data to a CSV file
  # data.to_csv(f"{ticker}_stock_data.csv") #This creates a new csv file each time, which takes alot of space
  # This line ensures the same file name is used every time, so it overwrites any existing file.
  data.to_csv("stock_data.csv", mode='w')

except:
  print ("Ticker not found")
