# Author: Carson Wolber, Date: 2-28-2024
import yfinance as yf
import pandas as pd
import sys
import time
from datetime import datetime, timedelta
import os
import signal

def signal_handler(sig, frame):
    """Handle the interrupt signal and exit cleanly."""
    print('\nInterrupt received, shutting down...')
    sys.exit(0)

# Register the signal handler for SIGINT (Ctrl+C)
signal.signal(signal.SIGINT, signal_handler)

def save_data(data, filename="stock_data.csv"):
    """Saves the downloaded stock data to a CSV file."""
    try:
        if os.path.exists(filename):
            os.remove(filename)  # Ensure we're writing fresh data
        data.to_csv(filename)
        print(f"Data saved successfully to {filename}")
    except Exception as e:
        print(f"Failed to save data to {filename}: {e}")

def calculate_moving_average(data, window=5):
    """Calculate moving average for the 'close' prices."""
    return data['close'].rolling(window=window).mean().iloc[-1]

def download_past_data(ticker):
    """Download the past 100 days of daily data."""
    end_date = datetime.now()
    start_date = end_date - timedelta(days=100)
    filename = "historical_data.csv"
    try:
        data = yf.download(ticker, start=start_date.strftime('%Y-%m-%d'), end=end_date.strftime('%Y-%m-%d'))
        if not data.empty:
            save_data(data, filename)
        else:
            print(f"No historical data found for {ticker}")
    except Exception as e:
        print(f"Failed to download past data for {ticker} with error: {e}")

def download_minute_data(ticker):
    """Download the minute-level data for the current day."""
    today = datetime.today().date()
    filename = "minute_data.csv"
    try:
        data = yf.download(tickers=ticker, period='1d', interval='1m') #yf.download(ticker, start=str(today), end=str(today), interval='1m')
        if not data.empty:
            save_data(data, filename)
            print(f"Minute data for {ticker} on {today} downloaded successfully.")
        else:
            print(f"No minute-level data found for {ticker} on {today}.")
            pd.DataFrame(columns=['Date', 'Open', 'High', 'Low', 'Close', 'Volume']).to_csv(filename)
            print(f"Empty {filename} created as no trading data was available.")
    except Exception as e:
        print(f"Error fetching minute-level data for {ticker} on {today}: {e}")
        pd.DataFrame(columns=['Date', 'Open', 'High', 'Low', 'Close', 'Volume']).to_csv(filename)
        print(f"Empty {filename} created due to an error.")

def download_data(ticker, start_date=None, end_date=None):
    """Download historical daily data for the given ticker within the specified date range."""
    interval = "1d"
    filename = "stock_data.csv"
    try:
        if start_date and end_date:
            data = yf.download(tickers=ticker, start=start_date, end=end_date, interval=interval)
        else:
            data = yf.download(tickers=ticker, period="10y", interval=interval)
        if not data.empty:
            save_data(data, filename)
        else:
            print(f"No data found for {ticker}")
    except Exception as e:
        print(f"Failed to download data for {ticker} with error: {e}")

def main():
    if len(sys.argv) == 2:
        ticker = sys.argv[1]
        download_data(ticker)  # Default to long-term historical data
    elif len(sys.argv) == 4:
        ticker = sys.argv[1]
        start_date = sys.argv[2]
        end_date = sys.argv[3]
        download_data(ticker, start_date, end_date)
    elif len(sys.argv) == 3 and sys.argv[2] == '--realtime':
        ticker = sys.argv[1]
        download_minute_data(ticker)  # Call to fetch minute-level data for today
    else:
        print("Incorrect usage of the script.")
        print("Usage: python download_data.py <ticker> [start_date end_date] [--realtime]")
        print("Example without dates: python download_data.py AAPL")
        print("Example with dates: python download_data.py AAPL 2020-01-01 2024-01-01")
        print("Example for real-time: python download_data.py AAPL --realtime")

if __name__ == "__main__":
    main()
