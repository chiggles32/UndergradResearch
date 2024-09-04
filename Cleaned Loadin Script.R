#Prepping futures import data

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

#consolidating to 10 min intervals

data = data |>
  as.xts(ymd_hms(x))

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
    mutate("Return" = Delt(data.Close), "CO" = wilderSum((sign(Return)*data.Volume*data.Close),n=12)/runMean(data.Volume*data.Close,n=12))


##Selecting CO data points i.e. 9:30 since indexed at end

CO_data = datat |>
  filter(Time == "09:20:00") |>
    select(Date, CO)

CO_data |>
  ggplot(aes(x = CO))+
    geom_histogram()

#making list of days


data_list = datat |>
  group_by(Date) |>
    group_split()




#Gets us to interval returns

#Tidying Index ETF Data

etf = read.table(file.choose(),sep = ",", header = F)

colnames(etf) = c("D/T", "Open", "High", "Low", "Close", "Volume")

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) >= hms("09:30:00") & hms(Time) < hms("12:00:00"))


##Visualizing Relationship



int_ret = etf|>
  filter(Time %in% c("09:30:00", "09:59:00")) |>
    group_by(Date) |>
      mutate(Int_Ret = Delt(Close))



Ret_data = int_ret |>
  filter(Time == "09:59:00")|>
    select(Date,Int_Ret)
head(Ret_data)


##Initial Analysis

Ret_data = as.data.frame(Ret_data)
CO_data = as.data.frame(CO_data)

x = unique(CO_data[,1])
y = unique(Ret_data[,1])
z = intersect(x,y)
q = length(z)


sampledaynum = sample(q,1000,replace = F)
sampledays = z[sampledaynum]

sRet = Ret_data |>
  filter(Date %in% sampledays)

sCO = CO_data |>
  filter(Date %in% sampledays)

merged = merge(sCO,sRet)
plot(merged[,2],merged[,3])
cor(merged[,2],merged[,3])



