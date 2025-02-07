""" get daily historical closing prices from Yahoo Finance """
import pandas as pd
from yfinance_util import get_historical_prices

pd.options.display.float_format = '{:.4f}'.format
symbols = ["SPY", "EFA", "EEM", "TLT"]
start_date = "1980-01-01"
end_date = "2025-02-06"
df = get_historical_prices(symbols, start_date, end_date)
df = df[[symbol for symbol in symbols]].dropna()
df.round(4).to_csv("prices.csv")
df_ret = df.pct_change()
print(df_ret.describe())
print("\ncorrelations:\n" + df_ret.corr().to_string())
