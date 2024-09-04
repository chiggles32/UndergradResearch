#Cleaning futures import data

library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)

data = read.table(file.choose(),sep = ",", header = F)

colnames(data) = c("D/T", "Open", "High", "Low", "Close", "Volume")

data = data |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) < hms("09:31:00") & hms(Time) > hms("00:00:00")) 

x = data[,1]

data = data |>
  select(Open, High, Low, Close, Volume)

data = data |>
  as.xts(ymd_hms(x))

#fix timing of consolidation

data = data |>
  to.period(period = "minutes", k = 10, indexAt = "startof")

x = index(data)

datat = as_tibble(data)

datat = datat |>
  mutate("D/T" = x) |>
    separate("D/T", c("Date","Time"), sep = " ", remove = F)

#calculating factors/indicators

datat = datat |>
  group_by(Date) |>
    mutate("Return" = Delt(data.Close))

datat = datat |>
  group_by(Date) |>
    mutate("CO" = wilderSum((sign(Return)*data.Volume),n=3)/runMean(data.Volume,n=3))

##Selecting CO data points i.e. 9:30

CO_data = datat |>
  filter(Time == "09:30:00") |>
    select(Date, CO)


#making list of days
#can drop unnecessary columns prior to make life easier

data_list = datat |>
  group_by(Date) |>
    group_split()




#Gets us to tidy version of returns at 10 minute intervals

#Tidying Index ETF Data
#Once converted to xts, tidyverse no bueno
etf
etf = read.table(file.choose(),sep = ",", header = F)

colnames(etf) = c("D/T", "Open", "High", "Low", "Close", "Volume")

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) >= hms("09:30:00") & hms(Time) < hms("12:00:00"))

y = etf[,1]

etf = etf |>
  select(Open, High, Low, Close, Volume)

etf = etf |>
  as.xts(ymd_hms(y))

etf = etf |>
  to.period(period = "minutes", k = 10, indexAt = "startof")

#Currently xts object

y = index(etf)

#Change back to tibble for easy manipulation

etft = as_tibble(etf)

etft = etft |>
  mutate("D/T" = y) |>
    separate("D/T", c("Date","Time"), sep = " ", remove = F)
    

#still need to calculate cumulative return at each step


int_ret = etft |>
  filter(Time %in% c("09:40:00", "11:50:00")) |>
    group_by(Date) |>
      mutate(Int_Ret = etf.Close/lag(etf.Close)-1)

Ret_data = int_ret |>
  filter(Time == "11:50:00") |>
    select(Date,Int_Ret)
Ret_data

#vol check
#     mutate("R" = Close/lag(Close)-1)


