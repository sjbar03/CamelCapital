import yfinance as yf
import pandas as pd
#@author Carson Wolber, 2-28-2024
#if we want to access data for all s&p500 for instance we'd do this
#since yfinance doesn't have good support for analyzing multiple stocks, I'm creating a sort of backdoor way of getting the list of 500 tickers without having to do it manually. 
sp500 = pd.read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[0]
#if we did want to just to use a specific subset of stocks use this:
# t = yf.Tickers['(ticker names we want')]

#change start/end to whatever range we want, here I just used 8 years from today.
start = '2016-28-24'
end = '2024-28-24'

#'Symbol is the name the wiki file uses for tickers so we have to access them using that key, then we just want to clean the ticker names. 
#For instance, some tickers use dots in wiki but yfinance uses dashes so we need to convert tickers to be yf complient 
sp500['Symbol'] = sp500['Symbol'].str.replace('.', '-')
#we should only have one row per ticker, this is a little pedantic but .unique() ensures this. 
sp500_tickers = sp500['Symbol'].unique().tolist()
#all this was just a prerq to clean ticker names and get our dates, now we can actually download this data using yf for our timeframe
# the .stack() is a pd method which in very simple terms will rearrange data to make it easier to work with. 
data = yf.download(tickers= sp500_tickers, start=start, end=end).stack()
#yf will give us, for each day in our period, adj close (adjusted closing price), closing price, high, low, open(starting price), and volume(number of shares traded)
# this just gives a name to the two "exogenous" columns we gave as input to create data, our date(which is every day in our timeframe) and ticker names
data.index.names = ['date', 'ticker']
#and then I'm just setting all those column headers to lower case so they're easier to reference
data.columns = data.columns.str.lower()
#we can then export this data as a csv which we can use in ocaml analagous to a3
data.to_csv("intraday_data.csv") 


