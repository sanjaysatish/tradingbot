# Readme for analysis

## SQ PYPL Pair trading

Basic pair trading strategy between SQ and PYPL over the lifespan of both tickers. This spreadsheet stores the opening and closing price of SQ and PYPL over a 5 yr period as well as the ratio of their prices (SQ/PYPL) at close each day. If the current ratio is greater than the previous 14 day average ratio then the strategy is long SQ, if the current ratio is less than the 14 average it is long PYPL. It then decides to buy a ticker based on the ratio at the previous days close and sells either at market open or close the next day. Historical profits are calculated both for going long on only one ticker as well as a long/short combo.

## R Script for SQ/PYPL Pair trading

This script utilizes an algorithm to backtest and implement the afformentioned pair trading strategy.

Using the daily z-score of the ratio for this pair, we implement a strategy to either buy or sell a ticker based on wether or not this score is within a certain threshold.

The following two conditions must be met to fulfill a signal:

1. The Z-score is within a certain range
2. Using a hypothesis test of time series data to test for stationarity in the ratio

Assuming both conditions are met then the algorithm will long the ticker indicated by the signal and short the other.

The following hypothesis test is calculated using the Augmented Dickey Fuller test - useing a significance level of 0.05 when calculating p-vales.

### Analysis

## Add stuff here to keep track of stuff
