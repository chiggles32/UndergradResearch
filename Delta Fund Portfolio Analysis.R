library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(tibbletime)
library(purrr)

port_creation = "2022-10-12"

positions  = read.csv(file.choose())

symbols = positions[,1]

weights = positions |> 
  mutate(Weight = Mkt.value/sum(Mkt.value)) |>
    pull(Weight)

getSymbols(positions$Symbol,from = "2020-01-01")

shortest = function(data){
  days = c()
  for (i in 1:length(symbols)){
    days[i] = length(get(symbols[i])[,6])
    }
  sl <<- days
}

shortest(positions)
  
ML = min(sl)

prices = matrix(,ML,length(symbols))

times = index(get(symbols[1])[(sl[1]-ML+1):sl[1],6])

for (i in 1:length(symbols)){
  prices[,i] = get(symbols[i])[(sl[i]-ML+1):sl[i],6]
  }

returns = Return.calculate(as.xts(prices,times))

getSymbols("SPY", from = times[1])

spy = Return.calculate(SPY[,6])

portfolio = Return.portfolio(returns,weights)
preb = Return.portfolio(returns,weights, rebalance_on = "days")

comp = cbind(portfolio, spy, preb)
colnames(comp) = c("Our Portfolio", "S&P 500", "Continuously Rebalanced")

chart.CumReturns(comp, legend.loc = "topright", main = "Portfolio Performance")

newport = comp["2022-10-12/"]



chart.CumReturns(newport, legend.loc = "topleft", main = "Portfolio Performance", )

charts.PerformanceSummary(newport, methods = "ModifiedES")

table.Distributions(newport)
StdDev.annualized(comp)
table.Distributions(comp)
chart.CaptureRatios(comp[,-2],comp[,2])
chart.CaptureRatios(returns,comp[,2])

