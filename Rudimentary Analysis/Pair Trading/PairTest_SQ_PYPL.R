# Script for testing pair trading strategy between SQ, PYPL 
# Author: Sanjay Satish @sanjaysatish
# Date Created: 12/14/2020

#===================
# Setup:
#===================

## Note: Code was adapted and inspired by Marco Nicolas Dibo; github: @mdibo
# Loading libraries 
library(tidyverse)
library(dplyr)
library(quantstrat)
library(tseries)
library(PerformanceAnalytics)
library(IKTrading)
library(FinancialInstrument)


#===================
# Getting Data:
#===================

# Loading in data via GitHub 
# Note: Ticker data from Yahoo Finance 

#data.dir1 <- "https://github.com/sanjaysatish/tradingbot/blob/main/Data/PYPL.csv"
#data.dir2 <- "https://github.com/sanjaysatish/tradingbot/blob/main/Data/SQ.csv"

.from <- '2015-11-19'
.to <- '2020-12-08'

pypl <- 'PYPL'
sq <- 'SQ'
# PYPL data
#setSymbolLookup.FI(base_dir=data.dir1, Symbols='PYPL')
getSymbols(pypl, from=.from, to=.to)

# SQ data
#setSymbolLookup.FI(base_dir=data.dir2, Symbols='SQ')
getSymbols(sq, from=.from, to=.to)

# Adding spread between tickers (SQ-PYPL)
spread <- OHLC(SQ)-OHLC(PYPL)
colnames(spread)<-c("open","high","low","close")

symbols <- c("spread")
currency("USD")
stock(symbols, currency = "USD", multiplier = 1)

chart <- chart_Series(spread)

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
startdate = '2015-11-19'
startequity = 1000

qs.strategy <- 'pair.SQPYPL'
initPortf(qs.strategy, symbols = symbols, initDate=startdate)

initAcct(qs.strategy, portfolios=qs.strategy, initDate=startdate,initEq=startequity)

initOrders(qs.strategy,initDate=startdate)

# Save 
strategy(qs.strategy, store = TRUE)

#rm.strat('pair.SQPYPL') 

ls(.blotter)

ls(.strategy) 

#===================
# Analysis & Indicators:
#===================

# Finding Z scores

# Function that calculates the ratio at close between the two tickers
# Input: 
Ratio <- function(tickers) { 
    x1 <- get(tickers[1])
    x2 <- get(tickers[2])
    rt <- log10(Cl(x1) / Cl(x2))
    colnames(rt) <- 'Price.Ratio'
    rt
}

# Calculate ratio 
PriceRatio <- Ratio(c(sq[1],pypl[1]))

# Function that calculates moving average of price ratio over a 14 trading day period 
# Input: Price Ratio as calculated by Ratio Function

MaRatio <- function(priceratio){
  nday = 14
  Mavg <- rollapply(priceratio, nday, mean)
  colnames(Mavg) <- 'Price.Ratio.MA'
  Mavg
}

# Calculate Moving Averages and append to df
Price.Ratio.MA <- MaRatio(PriceRatio)

# Calculate standard deviation of ratio and append to df
Price.Ratio.SD <- sd(PriceRatio)

# Function for calculating z-scores of price ratio based on Moving Averages as Population Average
# Inputs: Df with Ratio/MA/SD

ZScore <- function(df){
  x_i <- df$Price.Ratio
  mu <- df$Price.Ratio.MA
  sigma <- df$Price.Ratio.SD
  
  z <- (x_i-mu)/sigma
  
  colnames(z)<- 'Z.Score'
  z
}

# Hypothesis Testing - adapted from Augmented Dickey Fuller

ft2<-function(x){
  adf.test(x)$p.value
}

Pval <- function(x){
  
  Augmented.df <- rollapply(x, width = 14, ft2)
  colnames(Augmented.df) <- "P.Value"
  Augmented.df
}

P.Value <- Pval(PriceRatio)

add.indicator(strategy = qs.strategy, name = "ZScore", arguments =
                list(x=merge(PriceRatio,Price.Ratio.MA,Price.Ratio.SD)))

add.indicator(strategy = qs.strategy, name = "Pval", arguments =
                list(x=quote(PriceRatio)))

# Plotting Time Series of Z Score 
x = merge(PriceRatio,Price.Ratio.MA,Price.Ratio.SD) 
Z.Score <- ZScore(x)
plot(main = "Z-Score Time Series", xlab = "Date" , ylab = "Z-Score",Z.Score, type = "l" )
abline(h = .25, col = 2, lwd = 3 ,lty = 2)
abline(h = -.25, col = 3, lwd = 3 ,lty = 2)

#===================
# Optimization:
#===================

# Set significance level, set this to 1 if we want to disregard hypothesis testing
alpha = 1 

# Setting relatively arbitrary entry and exit levels based on Z-score chart:

buyThresh = -.5
sellThresh = -buyThresh
exitlong = .25
exitshort = .25

# Add signals, position limits, and rules for strategy:
  
add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold=buyThresh,
                                                             relationship="lt", cross=FALSE),label="longEntryZ")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="P.Value", threshold= alpha,
                                                           relationship="lt", cross=FALSE),label="PEntry")

add.signal(qs.strategy, name="sigAND",
           arguments=list(columns=c("longEntryZ", "PEntry"), cross=FALSE),
           label="longEntry")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitlong,
                                                           relationship="gt", cross=FALSE),label="longExit")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold=sellThresh,
                                                           relationship="gt", cross=FALSE),label="shortEntryZ")

add.signal(qs.strategy, name="sigAND", arguments=list(columns=c("shortEntryZ", "PEntry"), cross=FALSE),
           label="shortEntry")

add.signal(qs.strategy, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitshort,
                                                           relationship="lt", cross=FALSE),label="shortExit")

addPosLimit( portfolio = qs.strategy, # add position limit rules
             symbol = 'spread',
             timestamp = startdate,
             maxpos = 3000,
             longlevels = 1,
             minpos = -3000)

add.rule(qs.strategy, name='ruleSignal',arguments = list(sigcol="longEntry",
                                                         sigval=TRUE, orderqty=3000,  osFUN = osMaxPos, replace = FALSE, ordertype='market',
                                                         orderside='long', prefer = "open"), type='enter' )

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="shortEntry",
                                                          sigval=TRUE, orderqty=-3000,  osFUN = osMaxPos, replace = FALSE,ordertype='market',
                                                          orderside='short', prefer = "open"), type='enter')

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="longExit",
                                                          sigval=TRUE, orderqty= 'all', ordertype='market', orderside='short', prefer = "open"), type='exit')

add.rule(qs.strategy, name='ruleSignal', arguments = list(sigcol="shortExit",
                                                          sigval=TRUE, orderqty= 'all' , ordertype='market', orderside='long', prefer = "open"), type='exit')

summary(get.strategy(qs.strategy))

#===================
# Backtesting:
#===================

applyStrategy(strategy = qs.strategy, portfolios = qs.strategy, mktdata = spread)

tns <-getTxns(Portfolio=qs.strategy, Symbol= symbols)

#Update w/ fulfilled strategy 

updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

chart.P2 = function (Portfolio, Symbol, Dates = NULL, ..., TA = NULL)
{
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol))
    Symbol <- ls(Portfolio$symbols)[[1]]
  else Symbol <- Symbol[1]
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer))
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(freq$scale, seconds = {
    mult = 1
  }, minute = {
    mult = 60
  }, hourly = {
    mult = 3600
  }, daily = {
    mult = 86400
  }, {
    mult = 86400
  })
  if (!isTRUE(freq$frequency * mult == round(freq$frequency,
                                             0) * mult)) {
    n = round((freq$frequency/mult), 0) * mult
  }
  else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates))
    Dates <- paste(first(index(Prices)), last(index(Prices)),
                   sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades >
                                                           0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades <
                                                            0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1)
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position))))
    Position <- rbind(xts(0, order.by = first(index(Prices) -
                                                1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1)
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  else CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) -
                                                          1)), Drawdown)
  }
  else {
    Drawdown <- NULL
  }
  if (!is.null(Dates))
    Prices = Prices[Dates]
  chart_Series(Prices, name = Symbol, TA = TA, ...)
  if (!is.null(nrow(Buys)) && nrow(Buys) >= 1)
    (add_TA(Buys, pch = 2, type = "p", col = "green", on = 1))
  if (!is.null(nrow(Sells)) && nrow(Sells) >= 1)
    (add_TA(Sells, pch = 6, type = "p", col = "red", on = 1))
  if (nrow(Position) >= 1) {
    (add_TA(Positionfill, type = "h", col = "blue", lwd = 2))
    (add_TA(Position, type = "p", col = "orange", lwd = 2,
            on = 2))
  }
  if (!is.null(CumPL))
    (add_TA(CumPL, col = "darkgreen", lwd = 2))
  if (!is.null(Drawdown))
    (add_TA(Drawdown, col = "darkred", lwd = 2, yaxis = c(0,
                                                          -max(CumMax))))
  plot(current.chob())
}

chart.P2(qs.strategy, "spread", prefer = "close")

returns <- PortfReturns(qs.strategy)
charts.PerformanceSummary(returns, geometric=FALSE, wealth.index=TRUE, main = "Pair Strategy Returns")
