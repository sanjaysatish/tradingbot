# Script for testing pair trading strategy between SQ, PYPL 
# Author: Sanjay Satish @sanjaysatish
# Date Created: 12/14/2020

#===================
# Setup:
#===================

# Loading libraries 
library(tidyverse)
library(dplyr)
library(quantstrat)
library(tseries)
library(PerformanceAnalytics)

#===================
# Getting Data:
#===================

# Loading in data via GitHub 
# Note: Ticker data from Yahoo Finance 

data.dir1 <- "https://github.com/sanjaysatish/tradingbot/blob/main/Data/PYPL.csv"
data.dir2 <- "https://github.com/sanjaysatish/tradingbot/blob/main/Data/SQ.csv"

.from <- '2015-11-19'
.to <- '2020-12-08'

# PYPL data
setSymbolLookup.FI(base_dir=data.dir1, Symbols='PYPL')
getSymbols('PYPL', from=.from, to=.to)

# SQ data
setSymbolLookup.FI(base_dir=data.dir2, Symbols='SQ')
getSymbols('SQ', from=.from, to=.to)

# Adding spread between tickers (SQ-PYPL)
spread <- OHLC(SQ)-OHLC(PYPL)
colnames(spread)<-c("open","high","low","close")

symbols <- c("spread")
stock(symbols, currency = 'USD', multiplier = 1)

chart_Series(spread)

# Adding MA
add_TA(EMA(Cl(spread), n=20), on=1, col="blue", lwd=1.5)
legend(x=5, y=50, legend=c("EMA 20"),
       fill=c("blue"), bty="n")

#===================
# Initializing strategy:
#===================

# Initialize strategy, portfolio, etc. 
# .blotter holds portfolio and account objects and .strategy holds orderbook and strategy objects
.blotter <- new.env()
.strategy <- new.env()

# Assume starting on 01/02/2020 w/ $1000 in initial equity
qs.strategy <- 'pair.SQPYPL'
initPortf(qs.strategy, symbols = symbols, initDate=startdate)

startdate = '2020-01-02'
startequity = 1000
initAcct(qs.strategy, portfolios=qs.strategy, initDate=startdate,initEq=startequity)

initOrders(qs.strategy,initDate=startdate)

# Save 
strategy(qs.strategy, store = TRUE)

# rm.strat(pairStrat) 
ls(.blotter)

ls(.strategy) 

#===================
# Analysis & Indicators:
#===================

#===================
# Optimization:
#===================

#===================
# Backtesting:
#===================
